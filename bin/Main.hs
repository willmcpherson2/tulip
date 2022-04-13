module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Tulip (putResult)

main :: IO ()
main =
  getArgs >>= \case
    filename : _ -> do
      doesFileExist filename >>= \case
        True -> readFile filename >>= putResult
        False -> putStrLn "no such file exists"
    _ -> putStrLn "please supply a file"
