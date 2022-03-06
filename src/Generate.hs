module Generate (generate) where

import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe)
import Parse
import Text.Megaparsec (initialPos)

generate :: Ast -> Term
generate = \case
  Ast defs -> fromMaybe
    (TermError "no `main` found\n" $ initialPos "")
    (inline "main" defs)
  AstError msg pos -> TermError msg pos

inline :: String -> [Def] -> Maybe Term
inline ident defs = inlineTerm defs <$> resolve ident (reverse defs)

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
  var@(TermVar (Var name _)) -> case name of
    Ident ident _ -> fromMaybe var (inline ident defs)
    Blank{} -> var
    NameError msg pos -> TermError msg pos
  err@TermError{} -> err
