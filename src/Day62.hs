module Day62 (day62) where

import System.IO
import qualified Data.Set as Set
import qualified Data.List as List
import Data.List.Split

splitBlocks :: [String] -> [[String]]
splitBlocks = splitWhen (List.null)

commonAnswers :: [[Char]] -> Int
commonAnswers lines =
  let head:rest = fmap Set.fromList lines
   in length $ foldl Set.intersection head rest

numbersOfCommonAnswers :: [String] -> Int
numbersOfCommonAnswers = sum . fmap commonAnswers . splitBlocks

day62 :: IO ()
day62 = do
  contents <- readFile "input-day6.txt"
  putStrLn ("Day 6-2: " ++ (show $ numbersOfCommonAnswers $ lines contents))
