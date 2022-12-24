module Tulip (Pipeline (..), getPipeline, getResult, putPipeline, putResult) where

import Ast (Ast, Term)
import Display (Display (display))
import Eval (eval)
import Parse (parse)
import Report (Message, Report (report))

data Pipeline = Pipeline
  { source :: String,
    ast :: Ast,
    result :: Term,
    messages :: [Message]
  }
  deriving (Show)

getPipeline :: String -> Pipeline
getPipeline source =
  let ast = parse source
      result = eval 1000000 ast
      messages = case report source ast of
        [] -> report source result
        ms -> ms
   in Pipeline {source, ast, result, messages}

getResult :: String -> String
getResult source =
  let pipeline = getPipeline source
   in case messages pipeline of
        [] -> display $ result pipeline
        ms -> display ms

putResult :: String -> IO ()
putResult = putStrLn . getResult

putPipeline :: String -> IO ()
putPipeline s =
  let Pipeline {source, ast, messages, result} = getPipeline s
      lines =
        [ "Source:",
          source,
          "",
          "Ast:",
          display ast,
          "",
          "Result:",
          display result,
          "",
          "Errors:",
          display messages
        ]
   in putStr $ unlines lines
