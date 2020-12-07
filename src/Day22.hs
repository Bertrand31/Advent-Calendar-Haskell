module Day22 (day22) where

import System.IO
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List

data Policy = Policy { char :: Char, firstIndex :: Int, secondIndex :: Int } deriving Show

data Record = Record { policy :: Policy, password :: String } deriving Show

parsePolicy :: String -> Policy
parsePolicy str =
  let (firstIndexStr, rest) = span (/= '-') str
      (secondIndexStr, charRest) = span (/= ' ') (drop 1 rest)
  in Policy {
    char = (last charRest),
    firstIndex = (read firstIndexStr),
    secondIndex = (read secondIndexStr)
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
      map = Map.fromList $ zip [1..] pw
      cond1 = fmap (== (char pol)) $ Map.lookup (firstIndex pol) map
      cond2 = fmap (== (char pol)) $ Map.lookup (secondIndex pol) map
  in fromMaybe False $ (/=) <$> cond1 <*> cond2

day22 :: IO ()
day22 = do
    contents <- readFile "input-day2.txt"
    let records = fmap parseLine $ lines contents
    putStrLn $ "Day 2.2: " ++ (show . length . filter isValid) records
