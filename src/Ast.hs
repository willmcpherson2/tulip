module Ast
  ( Pos,
    Span,
    Ast (..),
    Def (..),
    Term (..),
    Name (..),
    Token (..),
    Tree (..),
    Error (..),
    GetSpan (..),
    Display (..),
  )
where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)

newtype Ast = Ast [Def]
  deriving (Show)

data Def
  = Def Span Name Term
  | DefError Error
  deriving (Show)

data Term
  = Fun Span Name Term
  | App Span Term Term
  | Var Span Name
  | TermError Error
  deriving (Show)

data Name
  = Ident Span (NonEmpty Char)
  | Blank Span
  | NameError Error
  deriving (Show)

--------------------------------------------------------------------------------

data Tree
  = ParenBranch Span [Tree]
  | BracketBranch Span [Tree]
  | Leaf Span (NonEmpty Char)
  | TreeError Error
  deriving (Show)

--------------------------------------------------------------------------------

data Token
  = OpenParen Span
  | CloseParen Span
  | OpenBracket Span
  | CloseBracket Span
  | Word Span (NonEmpty Char)
  deriving (Show)

--------------------------------------------------------------------------------

data Error
  = ExpectedDefToken Token
  | ExpectedCloseBracket Span
  | ExpectedCloseParen Span
  | ExpectedDefTree Tree
  | ExpectedNameTerm Tree
  | ExpectedName Tree
  | ExpectedParamBody Tree
  | ExpectedBody Tree
  | ExpectedTermTerm Tree
  | ExpectedTerm Tree
  | MainNotFound Span
  | EvaluatedHole Span
  | ApplicationOnSymbol Span (NonEmpty Char)
  | ApplicationOnHole Span
  deriving (Show)

--------------------------------------------------------------------------------

type Pos = Int

type Span = (Pos, Maybe Pos)

--------------------------------------------------------------------------------

class GetSpan a where
  getSpan :: a -> Span

  getStart :: a -> Pos
  getStart = fst . getSpan

  getEnd :: a -> Maybe Pos
  getEnd = snd . getSpan

instance GetSpan Def where
  getSpan = \case
    Def span _ _ -> span
    DefError e -> getSpan e

instance GetSpan Term where
  getSpan = \case
    Fun span _ _ -> span
    App span _ _ -> span
    Var span _ -> span
    TermError e -> getSpan e

instance GetSpan Name where
  getSpan = \case
    Ident span _ -> span
    Blank span -> span
    NameError e -> getSpan e

instance GetSpan Tree where
  getSpan = \case
    ParenBranch span _ -> span
    BracketBranch span _ -> span
    Leaf span _ -> span
    TreeError e -> getSpan e

instance GetSpan Token where
  getSpan = \case
    OpenParen span -> span
    CloseParen span -> span
    OpenBracket span -> span
    CloseBracket span -> span
    Word span _ -> span

instance GetSpan Error where
  getSpan = \case
    ExpectedDefToken t -> getSpan t
    ExpectedCloseBracket span -> span
    ExpectedCloseParen span -> span
    ExpectedDefTree tree -> getSpan tree
    ExpectedNameTerm tree -> getSpan tree
    ExpectedName tree -> getSpan tree
    ExpectedParamBody tree -> getSpan tree
    ExpectedBody tree -> getSpan tree
    ExpectedTermTerm tree -> getSpan tree
    ExpectedTerm tree -> getSpan tree
    MainNotFound span -> span
    EvaluatedHole span -> span
    ApplicationOnSymbol span _ -> span
    ApplicationOnHole span -> span

--------------------------------------------------------------------------------

class Display a where
  display :: a -> String

instance Display Ast where
  display (Ast defs) = intercalate "\n" (map display defs)

instance Display Def where
  display = \case
    Def _ name term -> "(" ++ display name ++ " " ++ display term ++ ")"
    DefError e -> display e

instance Display Term where
  display = \case
    Fun _ param body -> "[" ++ display param ++ " " ++ display body ++ "]"
    App _ l r -> "(" ++ display l ++ " " ++ display r ++ ")"
    Var _ name -> display name
    TermError e -> display e

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
    ExpectedDefTree{} ->
      "expected definition form: `(name term)`"
    ExpectedNameTerm{} ->
      "expected a name and a term in definition: `(name term)`"
    ExpectedName{} ->
      "expected a name"
    ExpectedParamBody{} ->
      "expected a parameter and a body in function, got an empty function `[]`"
    ExpectedBody{} ->
      "expected both a parameter and a body in function, but only got one"
    ExpectedTermTerm{} ->
      "expected some terms in application, got empty parens `()`"
    ExpectedTerm{} ->
      "expected another term in application"
    MainNotFound{} ->
      "expected a `main` definition but couldn't find one"
    EvaluatedHole{} ->
      "evaluated hole `_`, evaluation terminated"
    ApplicationOnSymbol{} ->
      "application on symbol, evaluation terminated"
    ApplicationOnHole{} ->
      "application on hole `_`, evaluation terminated"
