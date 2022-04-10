module Report (Report (..), Message (..), getMessages) where

import Ast hiding (Token)
import Data.List (intercalate)
import Data.List.Extra (splitOn)
import Display (Display (display))
import GetSpan (GetSpan (getSpan))
import Prelude hiding (error, lines)

class Report a where
  report :: a -> [Error]

instance Report Ast where
  report (Ast defs) = concatMap report defs

instance Report Def where
  report = \case
    Def _ name term -> report name ++ report term
    DefError e -> [e]

instance Report Term where
  report = \case
    Fun _ param term -> report param ++ report term
    App _ l r -> report l ++ report r
    Var _ name -> report name
    TermError e -> [e]

instance Report Name where
  report = \case
    NameError e -> [e]
    _ -> []

--------------------------------------------------------------------------------

data Message = Message
  { lineNum :: Pos
  , columnNum :: Pos
  , text :: String
  , error :: Error
  }
  deriving (Show)

instance Display Message where
  display Message{lineNum, columnNum, error, text} =
    intercalate
      "\n"
      [ displaySpan lineNum columnNum
      , display error
      , pointAt columnNum
      , text
      ]

instance Display [Message] where
  display = intercalate "\n\n" . map display

displaySpan :: Pos -> Pos -> String
displaySpan lineNum columnNum =
  "error at " ++ displayPos lineNum columnNum

displayPos :: Pos -> Pos -> [Char]
displayPos line column = show (line + 1) ++ ":" ++ show (column + 1)

pointAt :: Pos -> String
pointAt pos = replicate pos ' ' ++ "↓"

--------------------------------------------------------------------------------

getMessages :: String -> [Error] -> [Message]
getMessages source = map (getMessage source)

getMessage :: String -> Error -> Message
getMessage source error =
  let span = getSpan error
      Sections{leftLines, prefix, mid, suffix} = getSections span source
      lineNum = length leftLines
      columnNum = length prefix
      text = prefix ++ mid ++ suffix
   in Message{lineNum, columnNum, text, error}

data Sections = Sections
  { leftLines :: [String]
  , prefix :: String
  , mid :: String
  , suffix :: String
  }

getSections :: Span -> String -> Sections
getSections (start, end) source =
  let (left, right) = case end of
        Just end -> splitAt (end + 1) source
        Nothing -> ([], source)
      suffix = case splitOn "\n" right of
        [] -> ""
        x : _ -> x
      (left', mid) = splitAt start left
      (prefix, leftLines) = case reverse $ splitOn "\n" left' of
        [] -> ("", [])
        x : xs -> (x, xs)
   in Sections{leftLines, prefix, mid, suffix}
