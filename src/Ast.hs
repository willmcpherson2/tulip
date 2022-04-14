module Ast
  ( Pos,
    Span,
    Ast (..),
    Def (..),
    Term (..),
    Name (..),
    Token (..),
    Tree (..),
    Error (..),
  )
where

import Data.List.NonEmpty (NonEmpty)

newtype Ast = Ast [Def]
  deriving (Show)

data Def
  = Def Span Name Term
  | DefError Error
  deriving (Show)

data Term
  = Fun Span Name Term
  | App Span Term Term
  | Var Span Name
  | TermError Error
  deriving (Show)

data Name
  = Ident Span (NonEmpty Char)
  | Blank Span
  | NameError Error
  deriving (Show)

--------------------------------------------------------------------------------

data Tree
  = ParenBranch Span [Tree]
  | BracketBranch Span [Tree]
  | Leaf Span (NonEmpty Char)
  | TreeError Error
  deriving (Show)

--------------------------------------------------------------------------------

data Token
  = OpenParen Span
  | CloseParen Span
  | OpenBracket Span
  | CloseBracket Span
  | Word Span (NonEmpty Char)
  deriving (Show)

--------------------------------------------------------------------------------

data Error
  = ExpectedDefToken Token
  | ExpectedCloseBracket Span
  | ExpectedCloseParen Span
  | ExpectedDefTree Tree
  | ExpectedNameTerm Tree
  | ExpectedName Tree
  | ExpectedParamBody Tree
  | ExpectedBody Tree
  | ExpectedTermTerm Tree
  | ExpectedTerm Tree
  | MainNotFound Span
  | ApplicationOnSymbol Span (NonEmpty Char)
  | ApplicationOnHole Span
  | EvalLimit Span
  deriving (Show)

--------------------------------------------------------------------------------

type Pos = Int

type Span = (Pos, Maybe Pos)
