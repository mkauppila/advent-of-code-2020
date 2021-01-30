module Day1a where

import Split
import System.IO

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
