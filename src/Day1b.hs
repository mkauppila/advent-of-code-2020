module Day1b where

import Split
import System.IO

parseInput :: String -> [Int]
parseInput xs = map read $ filter notEmpty $ splitOn "\n" xs
  where
    notEmpty = not . null

puzzle :: String -> String
puzzle input =
  let ns = parseInput input
      pairs = filter (\(a, b, c) -> a /= b && a /= c) [(a, b, c) | a <- ns, b <- ns, c <- ns]
      (f, s, t) = head $ filter (\(a, b, c) -> a + b + c == 2020) pairs
   in show (f * s * t)
