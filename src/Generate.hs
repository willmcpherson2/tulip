module Generate (generate) where

import Ast
import Data.List.Extra (firstJust)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)

generate :: Ast -> Term
generate (Ast defs) =
  fromMaybe (TermError $ MainNotFound (0, Nothing)) (inline ('m' :| "ain") defs)

inline :: NonEmpty Char -> [Def] -> Maybe Term
inline ident defs = inlineTerm defs <$> resolve ident (reverse defs)

resolve :: NonEmpty Char -> [Def] -> Maybe Term
resolve ident = firstJust $ \case
  Def _ (Ident _ defIdent) term ->
    if ident == defIdent then Just term else Nothing
  _ -> Nothing

inlineTerm :: [Def] -> Term -> Term
inlineTerm defs = \case
  Fun span param body -> Fun span param (inlineTerm defs body)
  App span l r -> App span (inlineTerm defs l) (inlineTerm defs r)
  var@(Var _ name) -> case name of
    Ident _ ident -> fromMaybe var (inline ident defs)
    Blank{} -> var
    NameError e -> TermError e
  err@TermError{} -> err
