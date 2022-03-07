module Main (main) where

import Ast (Display(display))
import Eval (eval)
import Generate (generate)
import Parse (parse)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

data Pipeline = Pipeline
  { source :: String
  , ast :: String
  , term :: String
  , result :: String
  }
  deriving Show

main :: IO ()
main = getArgs >>= \case
  filename : _ -> do
    doesFileExist filename >>= \case
      True -> do
        source <- readFile filename
        putStrLn $ result $ compile source
      False -> putStrLn "no such file exists"
  _ -> putStrLn "please supply a file"

compile :: String -> Pipeline
compile source =
  let
    ast = parse source
    term = generate ast
    result = eval term
  in Pipeline
    { source
    , ast = display ast
    , term = display term
    , result = display result
    }

dump :: String -> IO ()
dump s =
  let
    Pipeline { source, ast, term, result } = compile s
    lines =
      ["Source:", source, "\nAst:", ast, "\nTerm:", term, "\nResult:", result]
  in mapM_ putStrLn (lines :: [String])
