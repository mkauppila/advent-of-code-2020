module Two where

import System.IO

-- TODO rather use this: https://hackage.haskell.org/package/split
-- or splitAt from prelude!! Nope, splitAt would not work at all!!!
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
      pairs = filter (\(a, b, c) -> a /= b && a /= c) [(a, b, c) | a <- ns, b <- ns, c <- ns]
      (f, s, t) = head $ filter (\(a, b, c) -> a + b + c == 2020) pairs
   in show (f * s * t)

-- how to write a test? I could refactor these easily
-- if I could  verify the output after each modification?

main = interact puzzle
