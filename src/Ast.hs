module Ast
  ( Ast(..)
  , Def(..)
  , Term(..)
  , Var(..)
  , Fun(..)
  , App(..)
  , Name(..)
  , GetPos(..)
  , Display(..)
  ) where

import Data.List (intercalate)
import Text.Megaparsec (SourcePos, sourcePosPretty)

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

--------------------------------------------------------------------------------

class Display a where
  display :: a -> String

instance Display Ast where
  display = \case
    Ast defs -> intercalate "\n" (map display defs)
    AstError msg pos -> displayErr msg pos

instance Display Def where
  display = \case
    Def name term _ -> "(" ++ display name ++ " " ++ display term ++ ")"
    DefError msg pos -> displayErr msg pos

instance Display Term where
  display = \case
    TermFun fun -> display fun
    TermApp app -> display app
    TermVar var -> display var
    TermError msg pos -> displayErr msg pos

instance Display Fun where
  display (Fun param body _) =
    "[" ++ display param ++ " " ++ display body ++ "]"

instance Display App where
  display (App l r _) = "(" ++ display l ++ " " ++ display r ++ ")"

instance Display Var where
  display (Var name _) = display name

instance Display Name where
  display = \case
    Ident s _ -> s
    Blank _ -> "_"
    NameError msg pos -> displayErr msg pos

displayErr :: String -> SourcePos -> String
displayErr msg pos = msg ++ "at " ++ sourcePosPretty pos
