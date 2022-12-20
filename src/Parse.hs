module Parse (parse) where

import Ast
import Combinators
import Control.Arrow (Arrow (arr), (<<<))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GetSpan (GetSpan (getEnd, getSpan))
import Parser (Parser)
import qualified Parser as P
import Stream (Stream (..))

parse :: String -> Ast
parse s = P.parse (ast <<< trees <<< tokens) (0, s)

ast :: Parser [Tree] Ast
ast = arr $ Ast . map (P.parse def)

def :: Parser Tree Def
def = arr $ \case
  tree@(ParenBranch span trees) -> case trees of
    [l, r] -> Def span (P.parse name l) (P.parse term r)
    _ -> DefError $ ExpectedNameTerm tree
  TreeError e -> DefError e
  tree -> DefError $ ExpectedDefTree tree

term :: Parser Tree Term
term = arr $ \case
  tree@(BracketBranch (start, end) trees) -> case trees of
    [] -> TermError $ ExpectedParamBody tree
    [tree] -> case tree of
      TreeError e -> TermError e
      _ -> TermError $ ExpectedBody tree
    param : tree : trees ->
      let assoc = \case
            body :| [] -> P.parse term body
            param :| tree : trees ->
              let body = assoc $ tree :| trees
               in Fun
                    (fst . getSpan $ param, snd . getSpan $ body)
                    (P.parse name param)
                    body
       in Fun (start, end) (P.parse name param) (assoc $ tree :| trees)
  tree@(ParenBranch span trees) -> case trees of
    [] -> TermError $ ExpectedTermTerm tree
    [tree] -> case tree of
      TreeError e -> TermError e
      _ -> TermError $ ExpectedTerm tree
    l : r : trees ->
      let assoc app = \case
            [] -> app
            r : trees -> assoc (App span app (P.parse term r)) trees
       in assoc (App span (P.parse term l) (P.parse term r)) trees
  leaf@(Leaf span _) -> Var span (P.parse name leaf)
  TreeError e -> TermError e

name :: Parser Tree Name
name = arr $ \case
  Leaf span s -> case s of
    '_' :| "" -> Blank span
    _ -> Ident span s
  TreeError e -> NameError e
  tree -> NameError $ ExpectedName tree

--------------------------------------------------------------------------------

trees :: Parser [Token] [Tree]
trees = star $ try tree <<|>> treeError

tree :: Parser [Token] (Maybe Tree)
tree = try parenBranch <<|>> try bracketBranch <<|>> leaf

parenBranch :: Parser [Token] (Maybe Tree)
parenBranch = runMaybeT $ do
  OpenParen (start, _) <- MaybeT takeToken
  ts <- lift $ star $ try tree
  close <- lift getToken
  case close of
    Just (CloseParen (_, end)) -> do
      lift takeToken
      pure $ ParenBranch (start, end) ts
    t -> pure $ TreeError $ ExpectedCloseParen (start, t >>= getEnd)

bracketBranch :: Parser [Token] (Maybe Tree)
bracketBranch = runMaybeT $ do
  OpenBracket (start, _) <- MaybeT takeToken
  ts <- lift $ star $ try tree
  close <- lift getToken
  case close of
    Just (CloseBracket (_, end)) -> do
      lift takeToken
      pure $ BracketBranch (start, end) ts
    t -> pure $ TreeError $ ExpectedCloseBracket (start, t >>= getEnd)

leaf :: Parser [Token] (Maybe Tree)
leaf = runMaybeT $ do
  Word span s <- MaybeT takeToken
  pure $ Leaf span s

treeError :: Parser [Token] (Maybe Tree)
treeError = runMaybeT $ do
  t <- MaybeT takeToken
  pure $ TreeError $ ExpectedDefToken t

--------------------------------------------------------------------------------

tokens :: Parser (Pos, String) [Token]
tokens = star token

token :: Parser (Pos, String) (Maybe Token)
token =
  try (keyword '(' OpenParen)
    <<|>> try (keyword ')' CloseParen)
    <<|>> try (keyword '[' OpenBracket)
    <<|>> try (keyword ']' CloseBracket)
    <<|>> try word
    <<|>> skip

skip :: Parser (Pos, String) (Maybe Token)
skip = runMaybeT $ MaybeT takeToken *> MaybeT token

keyword :: Char -> (Span -> Token) -> Parser (Pos, String) (Maybe Token)
keyword ch mk = runMaybeT $ do
  start <- lift getPos
  MaybeT $ matchM ch
  end <- lift getPos
  pure $ mk (start, Just $ end - 1)

word :: Parser (Pos, String) (Maybe Token)
word = runMaybeT $ do
  start <- lift getPos
  let isWordChar ch = not (isSpace ch) && notElem ch "()[]"
  s <- MaybeT $ plus $ try $ satisfyM isWordChar
  end <- lift getPos
  pure $ Word (start, Just $ end - 1) s
