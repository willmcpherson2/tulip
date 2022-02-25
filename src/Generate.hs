module Generate (Term(..), Fun(..), App(..), Var(..), Name(..), generate) where

import Data.Maybe (mapMaybe)
import Parse (Ast(Ast))
import qualified Parse as P

data Term
  = TermFun Fun
  | TermApp App
  | TermVar Var
  deriving Show

newtype Var = Var Name
  deriving Show

data Fun = Fun Name Term
  deriving Show

data App = App Term Term
  deriving Show

data Name
  = Name String
  | Blank
  deriving Show

generate :: Ast -> Term
generate = \case
  Ast defs -> inline "main" $ mapMaybe genDef defs
  _ -> inline "main" []

genDef :: P.Def -> Maybe (String, Term)
genDef = \case
  P.Def (P.Name name _) term _ -> genTerm term >>= (\term -> Just (name, term))
  _ -> Nothing

genTerm :: P.Term -> Maybe Term
genTerm = \case
  P.TermFun fun -> TermFun <$> genFun fun
  P.TermApp app -> TermApp <$> genApp app
  P.TermVar var -> TermVar <$> genVar var
  _ -> Nothing

genFun :: P.Fun -> Maybe Fun
genFun (P.Fun param body _) = case param of
  P.Name name _ -> Fun (Name name) <$> genTerm body
  _ -> Fun Blank <$> genTerm body

genApp :: P.App -> Maybe App
genApp (P.App l r _) = App <$> genTerm l <*> genTerm r

genVar :: P.Var -> Maybe Var
genVar (P.Var name _) = case name of
  P.Name name _ -> Just $ Var $ Name name
  P.Blank _ -> Just $ Var Blank
  _ -> Nothing

--------------------------------------------------------------------------------

inline :: String -> [(String, Term)] -> Term
inline name defs = case lookup name (reverse defs) of
  Just term -> inlineTerm defs term
  Nothing -> TermVar $ Var $ Name name

inlineTerm :: [(String, Term)] -> Term -> Term
inlineTerm defs = \case
  TermFun (Fun param body) -> TermFun $ Fun param $ inlineTerm defs body
  TermApp (App l r) -> TermApp $ App (inlineTerm defs l) (inlineTerm defs r)
  TermVar (Var var) -> case var of
    Name name -> inline name defs
    Blank -> TermVar $ Var Blank
