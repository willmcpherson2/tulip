module Eval (eval) where

import Parse

eval :: Term -> Term
eval = \case
  fun@TermFun{} -> fun
  TermApp app -> evalApp app
  var@(TermVar (Var name _)) -> case name of
    Ident{} -> var
    Blank blankPos -> TermError "evaluated hole\n" blankPos
    NameError msg pos -> TermError msg pos
  err@TermError{} -> err

evalApp :: App -> Term
evalApp (App l r pos) = case l of
  TermFun (Fun param body _) -> case param of
    Ident param _ -> eval $ replace param body r
    Blank{} -> eval body
    NameError msg pos -> TermError msg pos
  l@TermApp{} -> case eval l of
    err@TermError{} -> err
    l -> eval $ TermApp $ App l r pos
  TermVar (Var name _) -> case name of
    Ident ident pos ->
      TermError ("application on symbol `" ++ ident ++ "`\n") pos
    Blank pos -> TermError "application on hole\n" pos
    NameError msg pos -> TermError msg pos
  err@TermError{} -> err

replace :: String -> Term -> Term -> Term
replace param body term = case body of
  TermFun fun -> replaceFun param fun term
  TermApp app -> TermApp $ replaceApp param app term
  TermVar var -> replaceVar param var term
  err@TermError{} -> err

replaceFun :: String -> Fun -> Term -> Term
replaceFun param fun@(Fun param' body pos) term = case param' of
  ident@(Ident param' _) -> if param' == param
    then TermFun fun
    else TermFun $ Fun ident (replace param body term) pos
  blank@Blank{} -> TermFun $ Fun blank (replace param body term) pos
  NameError msg pos -> TermError msg pos

replaceApp :: String -> App -> Term -> App
replaceApp param (App l r pos) term =
  App (replace param l term) (replace param r term) pos

replaceVar :: String -> Var -> Term -> Term
replaceVar param var@(Var name _) term = case name of
  Ident ident _ -> if ident == param then term else TermVar var
  Blank{} -> TermVar var
  NameError msg pos -> TermError msg pos
