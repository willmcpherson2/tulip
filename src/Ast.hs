module Ast
  ( Pos
  , Ast(..)
  , Def(..)
  , Term(..)
  , Var(..)
  , Fun(..)
  , App(..)
  , Name(..)
  , Token(..)
  , Tree(..)
  , Error(..)
  , GetPos(..)
  , Display(..)
  ) where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)

data Ast
  = Ast [Def]
  | AstError Error
  deriving Show

data Def
  = Def Pos Name Term
  | DefError Error
  deriving Show

data Term
  = TermFun Fun
  | TermApp App
  | TermVar Var
  | TermError Error
  deriving Show

data Var = Var Pos Name
  deriving Show

data Fun = Fun Pos Name Term
  deriving Show

data App = App Pos Term Term
  deriving Show

data Name
  = Ident Pos (NonEmpty Char)
  | Blank Pos
  | NameError Error
  deriving Show

--------------------------------------------------------------------------------

data Tree
  = ParenBranch Pos [Tree]
  | BracketBranch Pos [Tree]
  | Leaf Pos (NonEmpty Char)
  | TreeError Error
  deriving Show

--------------------------------------------------------------------------------

data Token
  = OpenParen Pos
  | CloseParen Pos
  | OpenBracket Pos
  | CloseBracket Pos
  | Word Pos (NonEmpty Char)
  deriving Show

--------------------------------------------------------------------------------

data Error
  = ExpectedDefToken Token
  | ExpectedCloseBracket Pos
  | ExpectedCloseParen Pos
  | ExpectedDefTree Tree
  | ExpectedNameTerm Tree
  | ExpectedName Tree
  | ExpectedParamBody Tree
  | ExpectedBody Tree
  | ExpectedTermTerm Tree
  | ExpectedTerm Tree
  | MainNotFound Pos
  | EvaluatedHole Pos
  | ApplicationOnSymbol Pos (NonEmpty Char)
  | ApplicationOnHole Pos
  deriving Show

--------------------------------------------------------------------------------

type Pos = Int

--------------------------------------------------------------------------------

class GetPos a where
  getPos :: a -> Pos

instance GetPos Def where
  getPos = \case
    Def pos _ _ -> pos
    DefError e -> getPos e

instance GetPos Term where
  getPos = \case
    TermFun fun -> getPos fun
    TermApp app -> getPos app
    TermVar var -> getPos var
    TermError e -> getPos e

instance GetPos Fun where
  getPos (Fun pos _ _) = pos

instance GetPos App where
  getPos (App pos _ _) = pos

instance GetPos Var where
  getPos (Var pos _) = pos

instance GetPos Name where
  getPos = \case
    Ident pos _ -> pos
    Blank pos -> pos
    NameError e -> getPos e

instance GetPos Tree where
  getPos = \case
    ParenBranch pos _ -> pos
    BracketBranch pos _ -> pos
    Leaf pos _ -> pos
    TreeError e -> getPos e

instance GetPos Token where
  getPos = \case
    OpenParen pos -> pos
    CloseParen pos -> pos
    OpenBracket pos -> pos
    CloseBracket pos -> pos
    Word pos _ -> pos

instance GetPos Error where
  getPos = \case
    ExpectedDefToken t -> getPos t
    ExpectedCloseBracket pos -> pos
    ExpectedCloseParen pos -> pos
    ExpectedDefTree tree -> getPos tree
    ExpectedNameTerm tree -> getPos tree
    ExpectedName tree -> getPos tree
    ExpectedParamBody tree -> getPos tree
    ExpectedBody tree -> getPos tree
    ExpectedTermTerm tree -> getPos tree
    ExpectedTerm tree -> getPos tree
    MainNotFound pos -> pos
    EvaluatedHole pos -> pos
    ApplicationOnSymbol pos _ -> pos
    ApplicationOnHole pos -> pos

--------------------------------------------------------------------------------

class Display a where
  display :: a -> String

instance Display Ast where
  display = \case
    Ast defs -> intercalate "\n" (map display defs)
    AstError e -> display e

instance Display Def where
  display = \case
    Def _ name term -> "(" ++ display name ++ " " ++ display term ++ ")"
    DefError e -> display e

instance Display Term where
  display = \case
    TermFun fun -> display fun
    TermApp app -> display app
    TermVar var -> display var
    TermError e -> display e

instance Display Fun where
  display (Fun _ param body) =
    "[" ++ display param ++ " " ++ display body ++ "]"

instance Display App where
  display (App _ l r) = "(" ++ display l ++ " " ++ display r ++ ")"

instance Display Var where
  display (Var _ name) = display name

instance Display Name where
  display = \case
    Ident _ s -> toList s
    Blank{} -> "_"
    NameError e -> display e

instance Display Token where
  display = \case
    OpenParen{} -> "("
    CloseParen{} -> ")"
    OpenBracket{} -> "["
    CloseBracket{} -> "]"
    Word _ s -> toList s

instance Display Error where
  display = \case
    ExpectedDefToken t ->
      "expected definition form: `(name term)`, got `" ++ display t ++ "`"
    ExpectedCloseBracket{} ->
      "this opening bracket has no matching closing bracket `]`"
    ExpectedCloseParen{} ->
      "this opening paren has no matching closing paren `)`"
    ExpectedDefTree{} -> "expected definition form: `(name term)`"
    ExpectedNameTerm{} ->
      "expected a name and a term in definition: `(name term)`"
    ExpectedName{} -> "expected a name"
    ExpectedParamBody{} ->
      "expected a parameter and a body in function, got an empty function `[]`"
    ExpectedBody{} ->
      "expected both a parameter and a body in function, but only got one"
    ExpectedTermTerm{} ->
      "expected some terms in application, got empty parens `()`"
    ExpectedTerm{} -> "expected another term in application"
    MainNotFound{} -> "expected a `main` definition but couldn't find one"
    EvaluatedHole{} -> "evaluated hole `_`, evaluation terminated"
    ApplicationOnSymbol{} -> "application on symbol, evaluation terminated"
    ApplicationOnHole{} -> "application on hole `_`, evaluation terminated"
