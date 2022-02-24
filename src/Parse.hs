module Parse (Ast(..), Def(..), Term(..), Var(..), Fun(..), App(..), Name(..), parse) where

import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
  ( (<|>)
  , MonadParsec(eof, try, withRecovery)
  , ParseErrorBundle(bundlePosState)
  , Parsec
  , PosState(pstateSourcePos)
  , SourcePos
  , anySingle
  , errorBundlePretty
  , getSourcePos
  , many
  , parseErrorTextPretty
  , runParser
  , some
  )
import Text.Megaparsec.Char (alphaNumChar, char, space)

--------------------------------------------------------------------------------

data Ast
  = Ast [Def]
  | AstError String SourcePos
  deriving Show

data Def
  = Def Name Term SourcePos
  | DefError String SourcePos
  deriving Show

data Term
  = TermFun Fun
  | TermApp App
  | TermVar Var
  | TermError String SourcePos
  deriving Show

data Var = Var Name SourcePos
  deriving Show

data Fun = Fun Name Term SourcePos
  deriving Show

data App = App Term Term SourcePos
  deriving Show

data Name
  = Name String SourcePos
  | Blank SourcePos
  | NameError String SourcePos
  deriving Show

--------------------------------------------------------------------------------

class GetPos a where
  getPos :: a -> SourcePos

instance GetPos Def where
  getPos = \case
    Def _ _ pos -> pos
    DefError _ pos -> pos

instance GetPos Term where
  getPos = \case
    TermFun x -> getPos x
    TermApp x -> getPos x
    TermVar x -> getPos x
    TermError _ pos -> pos

instance GetPos Fun where
  getPos (Fun _ _ pos) = pos

instance GetPos App where
  getPos (App _ _ pos) = pos

instance GetPos Var where
  getPos (Var _ pos) = pos

instance GetPos Name where
  getPos = \case
    Name _ pos -> pos
    Blank pos -> pos
    NameError _ pos -> pos

--------------------------------------------------------------------------------

parse :: String -> Ast
parse = mkTotal AstError parseAst

type Parser = Parsec Void String

mkTotal :: (String -> SourcePos -> a) -> Parser a -> String -> a
mkTotal fromErr parser source = case runParser parser "" source of
  Left err ->
    let
      pos = pstateSourcePos $ bundlePosState err
      msg = errorBundlePretty err
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
  parseOpenParen
  name <- parseName
  term <- parseTerm
  parseCloseParen
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
  parseOpenBracket
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
    parseBody = parseTerm <* parseCloseBracket

parseApp :: Parser App
parseApp = do
  pos <- getSourcePos
  parseOpenParen
  l <- parseTerm
  r <- parseTerm
  parseInnerApp $ App l r pos
  where
    parseInnerApp :: App -> Parser App
    parseInnerApp app = try (parseAppTerminal app) <|> parseNestedApp app

    parseAppTerminal :: App -> Parser App
    parseAppTerminal app = parseCloseParen $> app

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
  ident <- some alphaNumChar
  space
  pure $ Name ident pos

parseOpenParen :: Parser ()
parseOpenParen = char '(' *> space

parseCloseParen :: Parser ()
parseCloseParen = char ')' *> space

parseOpenBracket :: Parser ()
parseOpenBracket = char '[' *> space

parseCloseBracket :: Parser ()
parseCloseBracket = char ']' *> space
