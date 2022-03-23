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

import Ast hiding (getPos)
import Combinators
import Control.Arrow ((<<<), Arrow(arr))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty((:|)))
import Parser (Parser)
import qualified Parser as P

parse :: String -> Ast
parse s = P.parse (ast <<< trees <<< tokens) (0, s)

ast :: Parser [Tree] Ast
ast = arr $ Ast . map (P.parse def)

def :: Parser Tree Def
def = arr $ \case
  tree@(ParenBranch pos trees) -> case trees of
    [l, r] -> Def (P.parse name l) (P.parse term r) pos
    _ -> DefError $ ExpectedNameTerm tree
  TreeError e -> DefError e
  tree -> DefError $ ExpectedDefTree tree

term :: Parser Tree Term
term = arr $ \case
  tree@(BracketBranch pos trees) -> case reverse trees of
    [] -> TermError $ ExpectedParamBody tree
    [_] -> TermError $ ExpectedBody tree
    body : param : params ->
      let
        assoc fun = \case
          [] -> fun
          param : params ->
            assoc (TermFun $ Fun (P.parse name param) fun pos) params
        body' = P.parse term body
        param' = P.parse name param
      in assoc (TermFun $ Fun param' body' pos) params
  tree@(ParenBranch pos trees) -> case trees of
    [] -> TermError $ ExpectedTermTerm tree
    [_] -> TermError $ ExpectedTerm tree
    l : r : rs ->
      let
        assoc app = \case
          [] -> app
          l : r -> assoc (App (TermApp app) (P.parse term l) pos) r
        l' = P.parse term l
        r' = P.parse term r
        app = App l' r' pos
      in TermApp $ assoc app rs
  leaf@(Leaf pos _) -> TermVar $ Var (P.parse name leaf) pos
  TreeError e -> TermError e

name :: Parser Tree Name
name = arr $ \case
  Leaf pos s -> case s of
    '_' :| "" -> Blank pos
    _ -> Ident s pos
  TreeError e -> NameError e
  tree -> NameError $ ExpectedName tree

--------------------------------------------------------------------------------

trees :: Parser [Token] [Tree]
trees = star $ try tree <<|>> treeError

tree :: Parser [Token] (Maybe Tree)
tree = try parenBranch <<|>> try bracketBranch <<|>> leaf

parenBranch :: Parser [Token] (Maybe Tree)
parenBranch = runMaybeT $ do
  OpenParen pos <- MaybeT takeToken
  ts <- lift $ star $ try tree
  close <- lift takeToken
  pure $ case close of
    Just CloseParen{} -> ParenBranch pos ts
    _ -> TreeError $ ExpectedCloseParen pos

bracketBranch :: Parser [Token] (Maybe Tree)
bracketBranch = runMaybeT $ do
  OpenBracket pos <- MaybeT takeToken
  ts <- lift $ star $ try tree
  close <- lift takeToken
  pure $ case close of
    Just CloseBracket{} -> BracketBranch pos ts
    _ -> TreeError $ ExpectedCloseBracket pos

leaf :: Parser [Token] (Maybe Tree)
leaf = runMaybeT $ do
  Word pos s <- MaybeT takeToken
  pure $ Leaf pos s

treeError :: Parser [Token] (Maybe Tree)
treeError = runMaybeT $ do
  t <- MaybeT takeToken
  pure $ TreeError $ ExpectedDefToken t

--------------------------------------------------------------------------------

tokens :: Parser (Pos, String) [Token]
tokens = star token

token :: Parser (Pos, String) (Maybe Token)
token =
  try openParen
    <<|>> try closeParen
    <<|>> try openBracket
    <<|>> try closeBracket
    <<|>> try word
    <<|>> skip

skip :: Parser (Pos, String) (Maybe Token)
skip = runMaybeT $ MaybeT takeToken *> MaybeT token

openParen :: Parser (Pos, String) (Maybe Token)
openParen = runMaybeT $ OpenParen <$> lift getPos <* MaybeT (matchM '(')

closeParen :: Parser (Pos, String) (Maybe Token)
closeParen = runMaybeT $ CloseParen <$> lift getPos <* MaybeT (matchM ')')

openBracket :: Parser (Pos, String) (Maybe Token)
openBracket = runMaybeT $ OpenBracket <$> lift getPos <* MaybeT (matchM '[')

closeBracket :: Parser (Pos, String) (Maybe Token)
closeBracket = runMaybeT $ CloseBracket <$> lift getPos <* MaybeT (matchM ']')

word :: Parser (Pos, String) (Maybe Token)
word =
  let isWordChar ch = not (isSpace ch) && notElem ch "()[]"
  in
    runMaybeT $ Word <$> lift getPos <*> MaybeT
      (plus $ try $ satisfyM isWordChar)
