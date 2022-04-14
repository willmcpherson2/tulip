module Eval (eval) where

import Ast
import Data.List.Extra (firstJust)
import Data.List.NonEmpty (NonEmpty ((:|)))

eval :: Ast -> Term
eval (Ast defs) =
  maybe
    (TermError $ MainNotFound (0, Nothing))
    (evalTerm defs)
    (resolve ('m' :| "ain") defs)

evalTerm :: [Def] -> Term -> Term
evalTerm defs = \case
  fun@Fun{} -> fun
  App span l r -> apply defs span l r
  var@(Var _ name) -> case name of
    Ident{} -> var
    Blank blankPos -> TermError $ EvaluatedHole blankPos
    NameError e -> TermError e
  err@TermError{} -> err

apply :: [Def] -> Span -> Term -> Term -> Term
apply defs span l r = case l of
  Fun _ param body -> case param of
    Ident _ param -> evalTerm defs (replace defs param body r)
    Blank{} -> evalTerm defs body
    NameError e -> TermError e
  l@App{} -> case evalTerm defs l of
    err@TermError{} -> err
    l -> evalTerm defs (App span l r)
  Var _ name -> case name of
    Ident span ident ->
      maybe
        (TermError $ ApplicationOnSymbol span ident)
        (\l -> apply defs span l r)
        (resolve ident defs)
    Blank span -> TermError $ ApplicationOnHole span
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
