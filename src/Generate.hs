module Generate (generate) where

import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe)
import Eval (throw, throwMsg)
import Parse

generate :: Ast -> Term
generate = \case
  Ast defs -> fromMaybe (throwMsg "main not found") (inline "main" defs)
  AstError msg pos -> throw pos msg

inline :: String -> [Def] -> Maybe Term
inline name defs = case resolve name (reverse defs) of
  Just term -> Just $ inlineTerm defs term
  Nothing -> Nothing

resolve :: String -> [Def] -> Maybe Term
resolve name = firstJust $ \case
  Def (Name defName _) term _ -> if name == defName then Just term else Nothing
  _ -> Nothing

inlineTerm :: [Def] -> Term -> Term
inlineTerm defs = \case
  TermFun (Fun param body pos) ->
    TermFun $ Fun param (inlineTerm defs body) pos
  TermApp (App l r pos) ->
    TermApp $ App (inlineTerm defs l) (inlineTerm defs r) pos
  TermVar (Var var pos) -> case var of
    Name name namePos -> case inline name defs of
      Just term -> term
      Nothing -> TermVar $ Var (Name name namePos) pos
    Blank blankPos -> TermVar $ Var (Blank blankPos) pos
    err -> TermVar $ Var err pos
  err -> err
