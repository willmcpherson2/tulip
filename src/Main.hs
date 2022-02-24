module Main (main) where

import Eval (eval)
import Generate (Code, generate)
import Parse (Ast, parse)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

data Compilation = Compilation
  { source :: String
  , ast :: Ast
  , code :: Code
  , result :: Code
  }
  deriving Show

main :: IO ()
main = getArgs >>= \case
  filename : _ -> do
    doesFileExist filename >>= \case
      True -> do
        source <- readFile filename
        pPrint $ compile source
      False -> putStrLn "no such file exists"
  _ -> putStrLn "please supply a file"

compile :: String -> Compilation
compile source =
  let
    ast = parse source
    code = generate ast
    result = eval code
  in Compilation { source, ast, code, result }
