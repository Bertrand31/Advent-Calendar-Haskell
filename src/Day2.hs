module Day2 (day2) where

import System.IO
import qualified Data.Map as Map
import qualified Data.List as List

data Policy = Policy { char :: Char, lowerBound :: Int, upperBound :: Int } deriving Show

data Record = Record { policy :: Policy, password :: String } deriving Show

parsePolicy :: String -> Policy
parsePolicy str =
  let (lowerBoundStr, rest) = span (/= '-') str
      (upperBoundStr, charRest) = span (/= ' ') (drop 1 rest)
  in Policy {
    char = (last charRest),
    lowerBound = (read lowerBoundStr),
    upperBound = (read upperBoundStr)
  }

parseLine :: String -> Record
parseLine str =
  let (policy, rest) = span (/= ':') str
      password = drop 2 rest
  in Record { policy = (parsePolicy policy), password = password }

isValid ::  Record -> Bool
isValid record =
  let pol = policy record
      pw = password record
      occurences = length $ filter (== char pol) pw
   in occurences >= (lowerBound pol) && occurences <= (upperBound pol)

day2 :: IO ()
day2 = do
    contents <- readFile "input-day2.txt"
    let records = fmap parseLine $ lines contents
    putStrLn $ show $ length $ filter isValid records
