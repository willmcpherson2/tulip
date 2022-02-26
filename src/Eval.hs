module Eval (eval, throw, throwMsg) where

import Parse
import Text.Megaparsec (SourcePos, sourcePosPretty)

eval :: Term -> Term
eval = \case
  TermFun fun -> TermFun fun
  TermApp app -> evalApp app
  TermVar (Var var pos) -> case var of
    Name name namePos -> TermVar $ Var (Name name namePos) pos
    Blank blankPos -> throw blankPos "evaluated hole"
    NameError msg pos -> throw pos msg
  TermError msg pos -> throw pos msg

throw :: SourcePos -> String -> a
throw pos msg = error $ "error at " ++ sourcePosPretty pos ++ "\n" ++ msg

throwMsg :: String -> a
throwMsg = error

evalApp :: App -> Term
evalApp (App l r pos) = case l of
  TermFun (Fun param body _) -> case param of
    Name param _ -> eval $ replace param body r
    Blank _ -> eval body
    NameError msg pos -> throw pos msg
  TermApp (App l' r' appPos) ->
    eval $ TermApp $ App (eval $ TermApp $ App l' r' appPos) r pos
  TermVar (Var name _) -> case name of
    Name name namePos ->
      throw namePos $ "application on symbol `" ++ name ++ "`"
    Blank blankPos -> throw blankPos "application on hole"
    NameError name pos -> throw pos name
  TermError msg pos -> throw pos msg

replace :: String -> Term -> Term -> Term
replace param body term = case body of
  TermFun fun -> TermFun $ replaceFun param fun term
  TermApp app -> TermApp $ replaceApp param app term
  TermVar var -> replaceVar param var term
  TermError msg pos -> throw pos msg

replaceFun :: String -> Fun -> Term -> Fun
replaceFun param fun term = case fun of
  Fun (Name param' namePos) body pos -> if param' == param
    then fun
    else Fun (Name param' namePos) (replace param body term) pos
  Fun (Blank blankPos) body pos ->
    Fun (Blank blankPos) (replace param body term) pos
  Fun (NameError msg pos) _ _ -> throw pos msg

replaceApp :: String -> App -> Term -> App
replaceApp param (App l r pos) term =
  App (replace param l term) (replace param r term) pos

replaceVar :: String -> Var -> Term -> Term
replaceVar param (Var name pos) term = case name of
  Name name namePos ->
    if name == param then term else TermVar $ Var (Name name namePos) pos
  Blank blankPos -> TermVar $ Var (Blank blankPos) pos
  NameError msg pos -> throw pos msg
