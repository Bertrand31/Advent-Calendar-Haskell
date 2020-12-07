module Day1 (day1) where

import System.IO
import qualified Data.Set as Set
import qualified Data.List as List

makeSumPairs :: Num a => Ord a => a -> [a] -> Maybe (a, a)
makeSumPairs target numbers =
  List.find (\x -> Set.member (snd x) numbersSet) pairs
  where numbersSet = Set.fromList numbers
        pairs = fmap (\x -> (x, target - x)) numbers

day1 :: IO ()
day1 = do
    contents <- readFile "input-day1.txt"
    let numbers = fmap read $ lines contents
    let pair = makeSumPairs 2020 numbers
    putStrLn $ show $ fmap (\x -> fst x * snd x) pair
