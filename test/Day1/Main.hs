module Main where

import Day1
import System.IO
import Test.HUnit
import Util

runPuzzle :: String -> (String -> String) -> IO String
runPuzzle filepath puzzle = do
  handle <- openFile filepath ReadMode
  input <- hGetContents handle
  let result = puzzle input
  return result

-- tests :: Test
-- tests =
--   let test1 = TestCase $ assertEqual "3 == 3" 3 foo
--       listList = TestList [TestLabel "t" test1]
--    in listList

instance Eq (IO String) where
  (==) (IO s) (IO p) = s == p

main =
  runTestTT $
    TestCase $
      assertEqual
        "result: 719796"
        (return "719796" :: (IO String))
        $ runPuzzle "./data/day1-input.txt" puzzle
