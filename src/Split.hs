module Split where

-- TODO rather use this: https://hackage.haskell.org/package/split
split :: Char -> String -> String -> [String]
split del temp "" = [temp]
split del temp (s : ss)
  | s == del = temp : (split del "" ss)
  | otherwise = split del (temp ++ [s]) ss

splitOn :: String -> String -> [String]
splitOn del strings = split (head del) "" strings
