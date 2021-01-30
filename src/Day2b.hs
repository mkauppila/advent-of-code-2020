module Day2b where

import Split
import System.IO

data PolicyRecord = PolicyRecord
  { lower :: Int,
    upper :: Int,
    letter :: Char,
    password :: String
  }
  deriving (Show)

createPolicyRecord :: String -> PolicyRecord
createPolicyRecord input = PolicyRecord {lower = lower - 1, upper = upper - 1, letter = letter, password = password}
  where
    splitted = splitOn " " input
    lower = read $ head $ splitOn "-" (head splitted)
    upper = read $ splitOn "-" (head splitted) !! 1
    letter = splitted !! 1 !! 0
    password = splitted !! 2

isPolicyValid :: PolicyRecord -> Bool
isPolicyValid policy =
  let l = letter policy
      first = password policy !! lower policy
      second = password policy !! upper policy
   in not (first == l && second == l) && (first == l || second == l)

parseInput :: String -> [PolicyRecord]
parseInput xs = map createPolicyRecord $ filter notEmpty $ splitOn "\n" xs
  where
    notEmpty = not . null

puzzle :: String -> String
puzzle input =
  let countOfValidPolicies = length (filter isPolicyValid (parseInput input))
   in show countOfValidPolicies
