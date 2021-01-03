module Main where

import Day1
import System.IO
import Test.HUnit

-- tests :: Test
-- tests =
--   let test1 = TestCase $ assertEqual "3 == 3" 3 foo
--       listList = TestList [TestLabel "t" test1]
--    in listList

readInput :: String -> IO Handle
readInput fn = openFile fn readMode

main = runTestTT $ TestCase (assertEqual "result: 719796" "719796" (puzzle "3\n44"))
