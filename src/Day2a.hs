module Day2a where

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
createPolicyRecord input = PolicyRecord {lower = lower, upper = upper, letter = letter, password = password}
  where
    splitted = splitOn " " input
    lower = read $ head $ splitOn "-" (head splitted)
    upper = read $ splitOn "-" (head splitted) !! 1
    letter = splitted !! 1 !! 0
    password = splitted !! 2

occurrenceCount :: Char -> String -> Int
occurrenceCount letter "" = 0
occurrenceCount letter password =
  if letter == head password
    then 1 + occurrenceCount letter (tail password)
    else 0 + occurrenceCount letter (tail password)

isPolicyValid :: PolicyRecord -> Bool
isPolicyValid policy =
  let count = occurrenceCount (letter policy) (password policy)
   in lower policy <= count && count <= upper policy

parseInput :: String -> [PolicyRecord]
parseInput xs = map createPolicyRecord $ filter notEmpty $ splitOn "\n" xs
  where
    notEmpty = not . null

puzzle :: String -> String
puzzle input =
  let countOfValidPolicies = length (filter isPolicyValid (parseInput input))
   in show countOfValidPolicies
