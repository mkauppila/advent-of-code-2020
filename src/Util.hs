module Util where

import System.IO

puzzleBootStrap :: String -> (String -> String) -> IO ()
puzzleBootStrap filepath puzzle = do
  handle <- openFile filepath ReadMode
  input <- hGetContents handle
  let result = puzzle input
  putStrLn $ "Result: " ++ result
  hClose handle
