module Parse
  ( Ast(..)
  , Def(..)
  , Term(..)
  , Var(..)
  , Fun(..)
  , App(..)
  , Name(..)
  , parse
  ) where

import Ast
import Data.Functor (($>))
import qualified Data.List.NonEmpty as N
import Data.Void (Void)
import Text.Megaparsec
  ( (<|>)
  , MonadParsec(eof, try, withRecovery)
  , ParseErrorBundle(bundleErrors, bundlePosState)
  , Parsec
  , PosState(pstateSourcePos)
  , SourcePos
  , anySingle
  , getSourcePos
  , many
  , noneOf
  , oneOf
  , parseErrorTextPretty
  , runParser
  , skipMany
  , some
  )
import Text.Megaparsec.Char (char)

parse :: String -> Ast
parse = mkTotal AstError parseAst

type Parser = Parsec Void String

mkTotal :: (String -> SourcePos -> a) -> Parser a -> String -> a
mkTotal fromErr parser source = case runParser parser "" source of
  Left err ->
    let
      pos = pstateSourcePos $ bundlePosState err
      msg = parseErrorTextPretty $ N.head $ bundleErrors err
    in fromErr msg pos
  Right ok -> ok

fallback :: (String -> SourcePos -> a) -> Parser a -> Parser a
fallback fromErr = withRecovery $ \err -> do
  pos <- getSourcePos
  anySingle
  space
  pure $ fromErr (parseErrorTextPretty err) pos

parseAst :: Parser Ast
parseAst = do
  space
  defs <- many parseDef
  eof
  pure $ Ast defs

parseDef :: Parser Def
parseDef = fallback DefError $ do
  pos <- getSourcePos
  openParen
  name <- parseName
  term <- parseTerm
  closeParen
  pure $ Def name term pos

parseTerm :: Parser Term
parseTerm =
  fallback TermError
    $ TermFun
    <$> try parseFun
    <|> TermApp
    <$> try parseApp
    <|> TermVar
    <$> parseVar

parseFun :: Parser Fun
parseFun = do
  pos <- getSourcePos
  openBracket
  Fun l r _ <- parseInnerFun
  pure $ Fun l r pos
  where
    parseInnerFun :: Parser Fun
    parseInnerFun = do
      pos <- getSourcePos
      param <- parseName
      body <- try parseBody <|> TermFun <$> parseInnerFun
      pure $ Fun param body pos

    parseBody :: Parser Term
    parseBody = parseTerm <* closeBracket

parseApp :: Parser App
parseApp = do
  pos <- getSourcePos
  openParen
  l <- parseTerm
  r <- parseTerm
  parseInnerApp $ App l r pos
  where
    parseInnerApp :: App -> Parser App
    parseInnerApp app = try (parseAppTerminal app) <|> parseNestedApp app

    parseAppTerminal :: App -> Parser App
    parseAppTerminal app = closeParen $> app

    parseNestedApp :: App -> Parser App
    parseNestedApp (App appL appR appPos) = do
      let l = TermApp $ App appL appR (getPos appL)
      r <- parseTerm
      parseInnerApp $ App l r appPos

parseVar :: Parser Var
parseVar = do
  pos <- getSourcePos
  name <- parseName
  pure $ Var name pos

parseName :: Parser Name
parseName = fallback NameError $ try parseBlank <|> parseIdent

parseBlank :: Parser Name
parseBlank = do
  pos <- getSourcePos
  char '_'
  space
  pure $ Blank pos

parseIdent :: Parser Name
parseIdent = do
  pos <- getSourcePos
  ident <- some ident
  space
  pure $ Ident ident pos

ident :: Parser Char
ident = noneOf (['_', '(', ')', '[', ']', ' ', '\t', '\n', '\r'] :: [Char])

openParen :: Parser ()
openParen = char '(' *> space

closeParen :: Parser ()
closeParen = char ')' *> space

openBracket :: Parser ()
openBracket = char '[' *> space

closeBracket :: Parser ()
closeBracket = char ']' *> space

space :: Parser ()
space = skipMany $ oneOf ([' ', '\t', '\n', '\r'] :: [Char])
