module Parse (Ast(..), Def(..), Term(..), Name(..), parse) where

import Ast
import Combinators hiding (getPos)
import qualified Combinators as C
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
    [l, r] -> Def pos (P.parse name l) (P.parse term r)
    _ -> DefError $ ExpectedNameTerm tree
  TreeError e -> DefError e
  tree -> DefError $ ExpectedDefTree tree

term :: Parser Tree Term
term = arr $ \case
  tree@(BracketBranch pos trees) -> case trees of
    [] -> TermError $ ExpectedParamBody tree
    [_] -> TermError $ ExpectedBody tree
    param : tree : trees ->
      let
        assoc = \case
          body :| [] -> P.parse term body
          param :| tree : trees ->
            Fun (getPos param) (P.parse name param) (assoc $ tree :| trees)
      in Fun pos (P.parse name param) (assoc $ tree :| trees)
  tree@(ParenBranch pos trees) -> case trees of
    [] -> TermError $ ExpectedTermTerm tree
    [_] -> TermError $ ExpectedTerm tree
    l : r : trees ->
      let
        assoc app = \case
          [] -> app
          r : trees -> assoc (App pos app (P.parse term r)) trees
      in assoc (App pos (P.parse term l) (P.parse term r)) trees
  leaf@(Leaf pos _) -> Var pos (P.parse name leaf)
  TreeError e -> TermError e

name :: Parser Tree Name
name = arr $ \case
  Leaf pos s -> case s of
    '_' :| "" -> Blank pos
    _ -> Ident pos s
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
openParen = runMaybeT $ OpenParen <$> lift C.getPos <* MaybeT (matchM '(')

closeParen :: Parser (Pos, String) (Maybe Token)
closeParen = runMaybeT $ CloseParen <$> lift C.getPos <* MaybeT (matchM ')')

openBracket :: Parser (Pos, String) (Maybe Token)
openBracket = runMaybeT $ OpenBracket <$> lift C.getPos <* MaybeT (matchM '[')

closeBracket :: Parser (Pos, String) (Maybe Token)
closeBracket =
  runMaybeT $ CloseBracket <$> lift C.getPos <* MaybeT (matchM ']')

word :: Parser (Pos, String) (Maybe Token)
word =
  let isWordChar ch = not (isSpace ch) && notElem ch "()[]"
  in
    runMaybeT $ Word <$> lift C.getPos <*> MaybeT
      (plus $ try $ satisfyM isWordChar)
