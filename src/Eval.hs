module Eval (eval) where

import Generate

eval :: Term -> Term
eval = \case
  TermFun fun -> TermFun fun
  TermApp app -> evalApp app
  TermVar (Var var) -> case var of
    Name name -> TermVar $ Var $ Name name
    Blank -> error "program terminated: evaluated hole"

evalApp :: App -> Term
evalApp (App l r) = case l of
  TermFun (Fun param body) -> case param of
    Name param -> eval $ replace param body r
    Blank -> eval body
  TermApp (App l' r') -> eval $ TermApp $ App (eval $ TermApp $ App l' r') r
  TermVar (Var name) -> case name of
    Name name ->
      error $ "program terminated: application on symbol `" ++ name ++ "`"
    Blank -> error "program terminated: application on hole"

replace :: String -> Term -> Term -> Term
replace param body term = case body of
  TermFun fun -> TermFun $ replaceFun param fun term
  TermApp app -> TermApp $ replaceApp param app term
  TermVar var -> replaceVar param var term

replaceFun :: String -> Fun -> Term -> Fun
replaceFun param fun term = case fun of
  Fun (Name param') body ->
    if param' == param then fun else Fun (Name param') $ replace param body term
  Fun Blank body -> Fun Blank $ replace param body term

replaceApp :: String -> App -> Term -> App
replaceApp param (App l r) term =
  App (replace param l term) (replace param r term)

replaceVar :: String -> Var -> Term -> Term
replaceVar param (Var name) term = case name of
  Name name -> if name == param then term else TermVar $ Var $ Name name
  Blank -> TermVar $ Var Blank
