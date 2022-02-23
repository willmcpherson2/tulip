module Main (main) where

import Parse (Ast, parse)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

data Compilation = Compilation
  { source :: String
  , ast :: Ast
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
  in Compilation { source, ast }
