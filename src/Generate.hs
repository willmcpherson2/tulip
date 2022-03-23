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
  Def _ (Ident _ defIdent) term ->
    if ident == defIdent then Just term else Nothing
  _ -> Nothing

inlineTerm :: [Def] -> Term -> Term
inlineTerm defs = \case
  TermFun (Fun pos param body) ->
    TermFun $ Fun pos param (inlineTerm defs body)
  TermApp (App pos l r) ->
    TermApp $ App pos (inlineTerm defs l) (inlineTerm defs r)
  var@(TermVar (Var _ name)) -> case name of
    Ident _ ident -> fromMaybe var (inline ident defs)
    Blank{} -> var
    NameError e -> TermError e
  err@TermError{} -> err
