module Day3 (day3) where

import System.IO

data Coordinate = Tree | Snow deriving (Show, Eq)

parseLine :: [Char] -> [Coordinate]
parseLine ('#':xs) = Tree : parseLine xs
parseLine ('.':xs) = Snow : parseLine xs
parseLine [] = []

isTree :: [Coordinate] -> Int -> Bool
isTree = ((==) Tree .) . (!!)

countTrees :: [String] -> Int
countTrees input =
  let forest = fmap (cycle . parseLine) input
      forestWithIndexes = zip [0,3..] forest
      hitTree = fmap (\(x, y) -> isTree y x) forestWithIndexes
  in length $ filter (== True) hitTree

day3 :: IO ()
day3 = do
    contents <- readFile "input-day3.txt"
    let blop = countTrees $ lines contents
    putStrLn $ "Day 3: " ++ (show blop)
