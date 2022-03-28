module Main (Pipeline(..), main, compile, dump) where

import Ast (Ast, Display(display), Term)
import Eval (eval)
import Generate (generate)
import Parse (parse)
import Report (Message, Report(report), getMessages)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

data Pipeline = Pipeline
  { source :: String
  , ast :: Ast
  , messages :: [Message]
  , term :: Term
  , result :: Term
  }
  deriving Show

main :: IO ()
main = getArgs >>= \case
  filename : _ -> do
    doesFileExist filename >>= \case
      True -> do
        source <- readFile filename
        let pipeline = compile source
        case messages pipeline of
          [] -> putStrLn $ display $ result pipeline
          ms -> putStrLn $ display ms
      False -> putStrLn "no such file exists"
  _ -> putStrLn "please supply a file"

compile :: String -> Pipeline
compile source =
  let
    ast = parse source
    messages = getMessages source (report ast)
    term = generate ast
    result = eval term
  in Pipeline { source, ast, messages, term, result }

dump :: String -> IO ()
dump s =
  let
    Pipeline { source, ast, messages, term, result } = compile s
    lines =
      [ "Source:"
      , source
      , ""
      , "Ast:"
      , display ast
      , ""
      , "Errors:"
      , display messages
      , ""
      , "Term:"
      , display term
      , ""
      , "Result:"
      , display result
      ]
  in putStr $ unlines lines
