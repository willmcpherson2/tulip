module Main (Pipeline(..), main, compile, dump) where

import Ast (Ast, Display(display), Term)
import Eval (eval)
import Generate (generate)
import Parse (parse)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

data Pipeline = Pipeline
  { source :: String
  , ast :: Ast
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
        putStrLn $ display $ result $ compile source
      False -> putStrLn "no such file exists"
  _ -> putStrLn "please supply a file"

compile :: String -> Pipeline
compile source =
  let
    ast = parse source
    term = generate ast
    result = eval term
  in Pipeline { source, ast, term, result }

dump :: String -> IO ()
dump s =
  let
    Pipeline { source, ast, term, result } = compile s
    lines =
      [ "Source:"
      , source
      , ""
      , "Ast:"
      , display ast
      , ""
      , "Term:"
      , display term
      , ""
      , "Result:"
      , display result
      ]
  in putStr $ unlines lines
