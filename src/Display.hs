module Display (Display (..)) where

import Ast
import Data.List (intercalate)
import Data.List.NonEmpty (toList)

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
