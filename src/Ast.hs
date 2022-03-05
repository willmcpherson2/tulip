module Ast
  ( Ast(..)
  , Def(..)
  , Term(..)
  , Var(..)
  , Fun(..)
  , App(..)
  , Name(..)
  , GetPos(..)
  ) where

import Text.Megaparsec (SourcePos)

data Ast
  = Ast [Def]
  | AstError String SourcePos
  deriving Show

data Def
  = Def Name Term SourcePos
  | DefError String SourcePos
  deriving Show

data Term
  = TermFun Fun
  | TermApp App
  | TermVar Var
  | TermError String SourcePos
  deriving Show

data Var = Var Name SourcePos
  deriving Show

data Fun = Fun Name Term SourcePos
  deriving Show

data App = App Term Term SourcePos
  deriving Show

data Name
  = Ident String SourcePos
  | Blank SourcePos
  | NameError String SourcePos
  deriving Show

--------------------------------------------------------------------------------

class GetPos a where
  getPos :: a -> SourcePos

instance GetPos Def where
  getPos = \case
    Def _ _ pos -> pos
    DefError _ pos -> pos

instance GetPos Term where
  getPos = \case
    TermFun x -> getPos x
    TermApp x -> getPos x
    TermVar x -> getPos x
    TermError _ pos -> pos

instance GetPos Fun where
  getPos (Fun _ _ pos) = pos

instance GetPos App where
  getPos (App _ _ pos) = pos

instance GetPos Var where
  getPos (Var _ pos) = pos

instance GetPos Name where
  getPos = \case
    Ident _ pos -> pos
    Blank pos -> pos
    NameError _ pos -> pos
