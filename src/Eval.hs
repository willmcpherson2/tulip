module Eval (eval) where

import Ast (Error(..), Span)
import Data.List.NonEmpty (NonEmpty)
import Parse

eval :: Term -> Term
eval = \case
  fun@Fun{} -> fun
  App span l r -> apply span l r
  var@(Var _ name) -> case name of
    Ident{} -> var
    Blank blankPos -> TermError $ EvaluatedHole blankPos
    NameError e -> TermError e
  err@TermError{} -> err

apply :: Span -> Term -> Term -> Term
apply span l r = case l of
  Fun _ param body -> case param of
    Ident _ param -> eval $ replace param body r
    Blank{} -> eval body
    NameError e -> TermError e
  l@App{} -> case eval l of
    err@TermError{} -> err
    l -> eval $ App span l r
  Var _ name -> case name of
    Ident span ident -> TermError $ ApplicationOnSymbol span ident
    Blank span -> TermError $ ApplicationOnHole span
    NameError e -> TermError e
  err@TermError{} -> err

replace :: NonEmpty Char -> Term -> Term -> Term
replace param body term = case body of
  Fun span param' body -> case param' of
    ident@(Ident _ param') -> if param' == param
      then Fun span ident body
      else Fun span ident (replace param body term)
    blank@Blank{} -> Fun span blank (replace param body term)
    NameError e -> TermError e
  App span l r -> App span (replace param l term) (replace param r term)
  Var span name -> case name of
    Ident _ ident -> if ident == param then term else Var span name
    Blank{} -> Var span name
    NameError e -> TermError e
  err@TermError{} -> err
