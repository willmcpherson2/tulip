module Eval (eval) where

import Parse

eval :: Term -> Term
eval = \case
  TermFun fun -> TermFun fun
  TermApp app -> evalApp app
  TermVar (Var var pos) -> case var of
    Ident ident identPos -> TermVar $ Var (Ident ident identPos) pos
    Blank blankPos -> TermError "evaluated hole" blankPos
    NameError msg pos -> TermError msg pos
  TermError msg pos -> TermError msg pos

evalApp :: App -> Term
evalApp (App l r pos) = case l of
  TermFun (Fun param body _) -> case param of
    Ident param _ -> eval $ replace param body r
    Blank _ -> eval body
    NameError msg pos -> TermError msg pos
  TermApp (App l' r' appPos) ->
    let l'' = eval $ TermApp $ App l' r' appPos
    in
      case l'' of
        err@(TermError _ _) -> err
        _ -> eval $ TermApp $ App l'' r pos
  TermVar (Var name _) -> case name of
    Ident ident identPos ->
      TermError ("application on symbol `" ++ ident ++ "`") identPos
    Blank blankPos -> TermError "application on hole" blankPos
    NameError msg pos -> TermError msg pos
  TermError msg pos -> TermError msg pos

replace :: String -> Term -> Term -> Term
replace param body term = case body of
  TermFun fun -> replaceFun param fun term
  TermApp app -> TermApp $ replaceApp param app term
  TermVar var -> replaceVar param var term
  TermError msg pos -> TermError msg pos

replaceFun :: String -> Fun -> Term -> Term
replaceFun param fun term = case fun of
  Fun (Ident param' paramPos) body pos -> if param' == param
    then TermFun fun
    else TermFun $ Fun (Ident param' paramPos) (replace param body term) pos
  Fun (Blank blankPos) body pos ->
    TermFun $ Fun (Blank blankPos) (replace param body term) pos
  Fun (NameError msg pos) _ _ -> TermError msg pos

replaceApp :: String -> App -> Term -> App
replaceApp param (App l r pos) term =
  App (replace param l term) (replace param r term) pos

replaceVar :: String -> Var -> Term -> Term
replaceVar param (Var name pos) term = case name of
  Ident ident identPos ->
    if ident == param then term else TermVar $ Var (Ident ident identPos) pos
  Blank blankPos -> TermVar $ Var (Blank blankPos) pos
  NameError msg pos -> TermError msg pos
