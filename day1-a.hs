module One where

import System.IO

-- TODO rather use this: https://hackage.haskell.org/package/split
-- or splitAt from prelude!!
split :: Char -> String -> String -> [String]
split del temp "" = [temp]
split del temp (s : ss)
  | s == del = temp : (split del "" ss)
  | otherwise = split del (temp ++ [s]) ss

splitOn :: String -> String -> [String]
splitOn del strings = split (head del) "" strings

parseInput :: String -> [Int]
parseInput xs = map read $ filter notEmpty $ splitOn "\n" xs
  where
    notEmpty = not . null

puzzle :: String -> String
puzzle input =
  let ns = parseInput input
      pairs = filter (\(a, b) -> a /= b) [(a, b) | a <- ns, b <- ns]
      (f, s) = head $ filter (\(a, b) -> a + b == 2020) pairs
   in show (f * s)

puzzleMain = interact puzzle
