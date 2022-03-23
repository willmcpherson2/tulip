module Generate (generate) where

import Ast
import Data.List.Extra (firstJust)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)

generate :: Ast -> Term
generate = \case
  Ast defs ->
    fromMaybe (TermError $ MainNotFound 0) (inline ('m' :| "ain") defs)
  AstError e -> TermError e

inline :: NonEmpty Char -> [Def] -> Maybe Term
inline ident defs = inlineTerm defs <$> resolve ident (reverse defs)

resolve :: NonEmpty Char -> [Def] -> Maybe Term
resolve ident = firstJust $ \case
  Def (Ident defIdent _) term _ ->
    if ident == defIdent then Just term else Nothing
  _ -> Nothing

inlineTerm :: [Def] -> Term -> Term
inlineTerm defs = \case
  TermFun (Fun param body pos) ->
    TermFun $ Fun param (inlineTerm defs body) pos
  TermApp (App l r pos) ->
    TermApp $ App (inlineTerm defs l) (inlineTerm defs r) pos
  var@(TermVar (Var name _)) -> case name of
    Ident ident _ -> fromMaybe var (inline ident defs)
    Blank{} -> var
    NameError e -> TermError e
  err@TermError{} -> err
