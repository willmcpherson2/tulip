module Eval (eval) where

import Ast (Error(..))
import Data.List.NonEmpty (NonEmpty)
import Parse

eval :: Term -> Term
eval = \case
  fun@TermFun{} -> fun
  TermApp app -> evalApp app
  var@(TermVar (Var name _)) -> case name of
    Ident{} -> var
    Blank blankPos -> TermError $ EvaluatedHole blankPos
    NameError e -> TermError e
  err@TermError{} -> err

evalApp :: App -> Term
evalApp (App l r pos) = case l of
  TermFun (Fun param body _) -> case param of
    Ident param _ -> eval $ replace param body r
    Blank{} -> eval body
    NameError e -> TermError e
  l@TermApp{} -> case eval l of
    err@TermError{} -> err
    l -> eval $ TermApp $ App l r pos
  TermVar (Var name _) -> case name of
    Ident ident pos -> TermError $ ApplicationOnSymbol pos ident
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
replaceFun param fun@(Fun param' body pos) term = case param' of
  ident@(Ident param' _) -> if param' == param
    then TermFun fun
    else TermFun $ Fun ident (replace param body term) pos
  blank@Blank{} -> TermFun $ Fun blank (replace param body term) pos
  NameError e -> TermError e

replaceApp :: NonEmpty Char -> App -> Term -> App
replaceApp param (App l r pos) term =
  App (replace param l term) (replace param r term) pos

replaceVar :: NonEmpty Char -> Var -> Term -> Term
replaceVar param var@(Var name _) term = case name of
  Ident ident _ -> if ident == param then term else TermVar var
  Blank{} -> TermVar var
  NameError e -> TermError e
