module Report (Report(..), Message(..), getMessages) where

import Ast hiding (Token)
import Data.List (intercalate)
import Data.List.Extra (firstJust)
import Data.Maybe (fromJust)
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
  { startLine :: Pos
  , startColumn :: Pos
  , endLine :: Pos
  , endColumn :: Pos
  , text :: String
  , error :: Error
  }
  deriving Show

instance Display Message where
  display Message { startLine, startColumn, endLine, endColumn, error, text } =
    intercalate
      "\n"
      [ displaySpan startLine startColumn endLine endColumn
      , display error
      , ""
      , pointAt startColumn
      , text
      ]

instance Display [Message] where
  display = intercalate "\n\n" . map display

displaySpan :: Pos -> Pos -> Pos -> Pos -> String
displaySpan startLine startColumn endLine endColumn =
  "error at "
    ++ displayPos startLine startColumn
    ++ "-"
    ++ displayPos endLine endColumn

displayPos :: Pos -> Pos -> [Char]
displayPos line column = show line ++ ":" ++ show column

pointAt :: Pos -> String
pointAt pos = replicate pos ' ' ++ "↓"

--------------------------------------------------------------------------------

type CharInfo = (Char, Info)

data Info = Info
  { infoPos :: Pos
  , infoLine :: Pos
  , infoColumn :: Pos
  }
  deriving Show

getMessages :: String -> [Error] -> [Message]
getMessages source = map (getMessage source)

getMessage :: String -> Error -> Message
getMessage source error =
  let
    span = getSpan error
    charInfos = getCharInfos source
    (start, end) = spanToInfoSpan span charInfos
    text = getLinesOf (infoLine start) (infoLine end) charInfos
  in Message
    { startLine = infoLine start
    , startColumn = infoColumn start
    , endLine = infoLine end
    , endColumn = infoColumn end
    , error
    , text
    }

getLinesOf :: Pos -> Pos -> [CharInfo] -> String
getLinesOf start end = map fst
  . filter (\(_, Info { infoLine }) -> infoLine >= start && infoLine <= end)

spanToInfoSpan :: Span -> [CharInfo] -> (Info, Info)
spanToInfoSpan (start, end) charInfos =
  (posToInfo (Just start) charInfos, posToInfo end charInfos)

posToInfo :: Maybe Pos -> [CharInfo] -> Info
posToInfo pos charInfos = case pos of
  Just pos -> fromJust $ firstJust
    (\(_, info) -> if infoPos info == pos then Just info else Nothing)
    charInfos
  Nothing -> snd $ last charInfos

-- FIXME non-unix lines

getCharInfos :: String -> [CharInfo]
getCharInfos = snd
  . foldl go (Info { infoPos = 0, infoLine = 0, infoColumn = 0 }, [])
  where
    go :: (Info, [CharInfo]) -> Char -> (Info, [CharInfo])
    go (info@Info { infoPos, infoLine, infoColumn }, infos) ch =
      let
        info' = case ch of
          '\n' -> Info
            { infoPos = infoPos + 1
            , infoLine = infoLine + 1
            , infoColumn = 0
            }
          _ -> Info
            { infoPos = infoPos + 1
            , infoLine = infoLine
            , infoColumn = infoColumn + 1
            }
      in (info', infos ++ [(ch, info)])
