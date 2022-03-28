{-# LANGUAGE FlexibleInstances #-}

module Report (Report(..), Message(..), getMessages) where

import Ast hiding (Token)
import Data.List (intercalate)
import Data.List.Split (splitWhen)
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
  { lines :: [(String, Pos, Pos)]
  , error :: Error
  , line :: Pos
  , column :: Pos
  }
  deriving Show

instance Display Message where
  display Message { lines, error, line, column } = intercalate
    "\n"
    [displayLocation line column, displayLines lines, display error]

instance Display [Message] where
  display = intercalate "\n\n" . map display

displayLines :: [(String, Pos, Pos)] -> String
displayLines =
  concatMap $ \(s, start, end) -> s ++ "\n" ++ displayUnderline start end

displayLocation :: Pos -> Pos -> [Char]
displayLocation line column = "error at " ++ show line ++ ":" ++ show column

displayUnderline :: Pos -> Pos -> String
displayUnderline start end =
  replicate start ' ' ++ replicate (end - start + 1) '^'

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
    errorSpan = getSpan error
    charInfos = getCharInfos source
    charInfoLines = getCharInfoLines charInfos
    errorLines = getErrorLines errorSpan charInfoLines
    lines = getErrorLinesWithSpans errorSpan errorLines
    (line, column) = errorIsAt errorSpan errorLines
  in Message { lines, error, line, column }

errorIsAt :: Span -> [[CharInfo]] -> (Pos, Pos)
errorIsAt (start, _) charInfos =
  let
    flat = concat charInfos
    infos = map snd flat
    line = head $ map infoLine infos
    pos = head $ map infoPos infos
    column = start - pos
  in (line, column)

getErrorLinesWithSpans :: Span -> [[CharInfo]] -> [(String, Pos, Pos)]
getErrorLinesWithSpans (start, end) = map go
  where
    go :: [CharInfo] -> (String, Pos, Pos)
    go charInfos =
      let
        line = map fst charInfos
        Info { infoPos = linePos } = head $ map snd charInfos
        errorStart = start - linePos
        errorEnd = maybe (length charInfos) (\end -> end - linePos) end
      in (line, errorStart, errorEnd)

getErrorLines :: Span -> [[CharInfo]] -> [[CharInfo]]
getErrorLines (start, end) = filter go
  where
    go :: [CharInfo] -> Bool
    go =
      any $ \(_, Info { infoPos }) ->
        infoPos >= start && maybe True (infoPos <=) end

getCharInfoLines :: [CharInfo] -> [[CharInfo]]
getCharInfoLines = splitWhen $ \case
  ('\n', _) -> True
  _ -> False

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
