module Generate (Code, generate) where

import Data.Maybe (mapMaybe)
import Parse (Ast(Ast))
import qualified Parse as P

newtype Code = Code [Def]
  deriving Show

data Def = Def Name Term
  deriving Show

data Term
  = TermFun Fun
  | TermApp App
  | TermVar Var
  deriving Show

data Fun = Fun Name Term
  deriving Show

data App = App Term Term
  deriving Show

newtype Var = Var Name
  deriving Show

data Name
  = Name String
  | Blank
  deriving Show

generate :: Ast -> Code
generate = \case
  Ast defs -> Code $ mapMaybe genDef defs
  _ -> Code []

genDef :: P.Def -> Maybe Def
genDef = \case
  P.Def name term _ -> Def <$> genName name <*> genTerm term
  _ -> Nothing

genTerm :: P.Term -> Maybe Term
genTerm = \case
  P.TermFun fun -> TermFun <$> genFun fun
  P.TermApp app -> TermApp <$> genApp app
  P.TermVar var -> TermVar <$> genVar var
  _ -> Nothing

genVar :: P.Var -> Maybe Var
genVar (P.Var name _) = Var <$> genName name

genApp :: P.App -> Maybe App
genApp (P.App l r _) = App <$> genTerm l <*> genTerm r

genFun :: P.Fun -> Maybe Fun
genFun (P.Fun param body _) = Fun <$> genName param <*> genTerm body

genName :: P.Name -> Maybe Name
genName = \case
  P.Name name _ -> Just $ Name name
  P.Blank _ -> Just Blank
  _ -> Nothing
