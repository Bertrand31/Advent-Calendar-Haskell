module Day32 (day32) where

import System.IO
import Data.Function ((&))

data Coordinate = Tree | Snow deriving Eq

parseLine :: [Char] -> [Coordinate]
parseLine ('#':xs) = Tree : parseLine xs
parseLine ('.':xs) = Snow : parseLine xs
parseLine [] = []

isTree :: Int -> [Coordinate] -> Bool
isTree = ((==) Tree .) . (flip (!!))

parseForest :: [String] -> [[Coordinate]]
parseForest = fmap (cycle . parseLine)

keepEvery :: Int -> [a] -> [a]
keepEvery n list
  | length list >= n = head list : keepEvery n (drop n list)
  | otherwise        = list

countTrees :: Int -> Int -> [[Coordinate]] -> Int
countTrees rightStep downStep forest =
  let cleanForest = keepEvery downStep $ drop downStep forest
      forestWithIndexes = zip [rightStep,(rightStep*2)..] cleanForest
      hitTree = fmap (uncurry isTree) forestWithIndexes
   in hitTree & filter (== True) & length

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

day32 :: IO ()
day32 = do
    contents <- readFile "input-day3.txt"
    let forest = parseForest $ lines contents
    let results = fmap (\(x, y) -> countTrees x y forest) slopes
    putStrLn "Day 3-2: " <>
      (foldMap (putStrLn . show) $ zip slopes results) <>
      (putStrLn ("Final: " ++ show (foldl (*) 1 results)))
