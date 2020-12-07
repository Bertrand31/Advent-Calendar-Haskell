module Day6 (day6) where

import System.IO
import qualified Data.Set as Set
import qualified Data.List as List
import Data.List.Split

countUniq :: Ord a => [a] -> Int
countUniq = length . Set.fromList

splitBlocks :: [String] -> [[String]]
splitBlocks = splitWhen (List.null)

countUniqInBlock :: [String] -> Int
countUniqInBlock = countUniq . mconcat

uniqSum :: [String] -> Int
uniqSum = sum . (fmap countUniqInBlock) . splitBlocks

day6 :: IO ()
day6 = do
    contents <- readFile "input-day6.txt"
    let sum = uniqSum $ lines contents
    putStrLn ("Day 6: " ++ show sum)
