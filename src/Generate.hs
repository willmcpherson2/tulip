module Generate (generate) where

import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe)
import Parse

generate :: Ast -> Term
generate = \case
  Ast defs -> fromMaybe (TermError "" undefined) (inline "main" defs)
  AstError msg pos -> TermError msg pos

inline :: String -> [Def] -> Maybe Term
inline ident defs = case resolve ident (reverse defs) of
  Just term -> Just $ inlineTerm defs term
  Nothing -> Nothing

resolve :: String -> [Def] -> Maybe Term
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
  TermVar (Var var pos) -> case var of
    Ident ident identPos -> case inline ident defs of
      Just term -> term
      Nothing -> TermVar $ Var (Ident ident identPos) pos
    Blank blankPos -> TermVar $ Var (Blank blankPos) pos
    err -> TermVar $ Var err pos
  err -> err
