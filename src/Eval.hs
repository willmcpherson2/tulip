module Eval (eval) where

import Ast (Error(..))
import Data.List.NonEmpty (NonEmpty)
import Parse

eval :: Term -> Term
eval = \case
  fun@TermFun{} -> fun
  TermApp app -> evalApp app
  var@(TermVar (Var _ name)) -> case name of
    Ident{} -> var
    Blank blankPos -> TermError $ EvaluatedHole blankPos
    NameError e -> TermError e
  err@TermError{} -> err

evalApp :: App -> Term
evalApp (App pos l r) = case l of
  TermFun (Fun _ param body) -> case param of
    Ident _ param -> eval $ replace param body r
    Blank{} -> eval body
    NameError e -> TermError e
  l@TermApp{} -> case eval l of
    err@TermError{} -> err
    l -> eval $ TermApp $ App pos l r
  TermVar (Var _ name) -> case name of
    Ident pos ident -> TermError $ ApplicationOnSymbol pos ident
    Blank pos -> TermError $ ApplicationOnHole pos
    NameError e -> TermError e
  err@TermError{} -> err

replace :: NonEmpty Char -> Term -> Term -> Term
replace param body term = case body of
  TermFun fun -> replaceFun param fun term
  TermApp app -> TermApp $ replaceApp param app term
  TermVar var -> replaceVar param var term
  err@TermError{} -> err

replaceFun :: NonEmpty Char -> Fun -> Term -> Term
replaceFun param fun@(Fun pos param' body) term = case param' of
  ident@(Ident _ param') -> if param' == param
    then TermFun fun
    else TermFun $ Fun pos ident (replace param body term)
  blank@Blank{} -> TermFun $ Fun pos blank (replace param body term)
  NameError e -> TermError e

replaceApp :: NonEmpty Char -> App -> Term -> App
replaceApp param (App pos l r) term =
  App pos (replace param l term) (replace param r term)

replaceVar :: NonEmpty Char -> Var -> Term -> Term
replaceVar param var@(Var _ name) term = case name of
  Ident _ ident -> if ident == param then term else TermVar var
  Blank{} -> TermVar var
  NameError e -> TermError e
