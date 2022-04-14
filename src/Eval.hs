module Eval (eval) where

import Ast
import Data.List.Extra (firstJust)
import Data.List.NonEmpty (NonEmpty ((:|)))

eval :: Int -> Ast -> Term
eval n (Ast defs) =
  maybe
    (TermError $ MainNotFound (0, Nothing))
    (evalTerm n defs)
    (resolve ('m' :| "ain") defs)

evalTerm :: Int -> [Def] -> Term -> Term
evalTerm n defs term = case term of
  App span l r ->
    if n <= 0
      then TermError $ EvalLimit span
      else apply (n - 1) defs span l r
  Var _ (NameError e) -> TermError e
  term -> term

apply :: Int -> [Def] -> Span -> Term -> Term -> Term
apply n defs span l r = case l of
  Fun _ param body -> case param of
    Ident _ param -> evalTerm n defs (replace defs param body r)
    Blank{} -> evalTerm n defs body
    NameError e -> TermError e
  l@App{} -> case evalTerm n defs l of
    err@TermError{} -> err
    l -> evalTerm n defs (App span l r)
  Var _ name -> case name of
    Ident _ ident ->
      maybe
        (TermError $ ApplicationOnSymbol span ident)
        (\l -> evalTerm n defs (App span l r))
        (resolve ident defs)
    Blank _ -> TermError $ ApplicationOnHole span
    NameError e -> TermError e
  err@TermError{} -> err

replace :: [Def] -> NonEmpty Char -> Term -> Term -> Term
replace defs param body term = case body of
  Fun span param' body -> case param' of
    ident@(Ident _ param') ->
      if param' == param
        then Fun span ident body
        else Fun span ident (replace defs param body term)
    blank@Blank{} -> Fun span blank (replace defs param body term)
    NameError e -> TermError e
  App span l r -> App span (replace defs param l term) (replace defs param r term)
  Var span name -> case name of
    Ident _ ident -> if ident == param then term else Var span name
    Blank{} -> Var span name
    NameError e -> TermError e
  err@TermError{} -> err

--------------------------------------------------------------------------------

resolve :: NonEmpty Char -> [Def] -> Maybe Term
resolve ident = firstJust $ \case
  Def _ (Ident _ defIdent) term ->
    if ident == defIdent then Just term else Nothing
  _ -> Nothing
