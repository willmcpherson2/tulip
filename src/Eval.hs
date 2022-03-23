module Eval (eval) where

import Ast (Error(..), Pos)
import Data.List.NonEmpty (NonEmpty)
import Parse

eval :: Term -> Term
eval = \case
  fun@Fun{} -> fun
  App pos l r -> apply pos l r
  var@(Var _ name) -> case name of
    Ident{} -> var
    Blank blankPos -> TermError $ EvaluatedHole blankPos
    NameError e -> TermError e
  err@TermError{} -> err

apply :: Pos -> Term -> Term -> Term
apply pos l r = case l of
  Fun _ param body -> case param of
    Ident _ param -> eval $ replace param body r
    Blank{} -> eval body
    NameError e -> TermError e
  l@App{} -> case eval l of
    err@TermError{} -> err
    l -> eval $ App pos l r
  Var _ name -> case name of
    Ident pos ident -> TermError $ ApplicationOnSymbol pos ident
    Blank pos -> TermError $ ApplicationOnHole pos
    NameError e -> TermError e
  err@TermError{} -> err

replace :: NonEmpty Char -> Term -> Term -> Term
replace param body term = case body of
  Fun pos param' body -> case param' of
    ident@(Ident _ param') -> if param' == param
      then Fun pos ident body
      else Fun pos ident (replace param body term)
    blank@Blank{} -> Fun pos blank (replace param body term)
    NameError e -> TermError e
  App pos l r -> App pos (replace param l term) (replace param r term)
  Var pos name -> case name of
    Ident _ ident -> if ident == param then term else Var pos name
    Blank{} -> Var pos name
    NameError e -> TermError e
  err@TermError{} -> err
