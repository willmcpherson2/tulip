module GetSpan (GetSpan (..)) where

import Ast

class GetSpan a where
  getSpan :: a -> Span

  getStart :: a -> Pos
  getStart = fst . getSpan

  getEnd :: a -> Maybe Pos
  getEnd = snd . getSpan

instance GetSpan Def where
  getSpan = \case
    Def span _ _ -> span
    DefError e -> getSpan e

instance GetSpan Term where
  getSpan = \case
    Fun span _ _ -> span
    App span _ _ -> span
    Var span _ -> span
    TermError e -> getSpan e

instance GetSpan Name where
  getSpan = \case
    Ident span _ -> span
    Blank span -> span
    NameError e -> getSpan e

instance GetSpan Tree where
  getSpan = \case
    ParenBranch span _ -> span
    BracketBranch span _ -> span
    Leaf span _ -> span
    TreeError e -> getSpan e

instance GetSpan Token where
  getSpan = \case
    OpenParen span -> span
    CloseParen span -> span
    OpenBracket span -> span
    CloseBracket span -> span
    Word span _ -> span

instance GetSpan Error where
  getSpan = \case
    ExpectedDefToken t -> getSpan t
    ExpectedCloseBracket span -> span
    ExpectedCloseParen span -> span
    ExpectedDefTree tree -> getSpan tree
    ExpectedNameTerm tree -> getSpan tree
    ExpectedName tree -> getSpan tree
    ExpectedParamBody tree -> getSpan tree
    ExpectedBody tree -> getSpan tree
    ExpectedTermTerm tree -> getSpan tree
    ExpectedTerm tree -> getSpan tree
    MainNotFound span -> span
    ApplicationOnSymbol span _ -> span
    ApplicationOnHole span -> span
