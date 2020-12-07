module Day4 (day4) where

import System.IO
import qualified Data.List as List
import qualified Data.Map as Map
import Data.List.Split

newtype Passport = Passport { insides :: (Map.Map String String) } deriving Show

splitChunks :: [String] -> [String]
splitChunks = (fmap (List.intercalate " ")) . (splitWhen (List.null))

makeKeyValue :: String -> (String, String)
makeKeyValue str =
  let key:value:_ = splitOn ":" str
   in (key, value)

makePassport :: String -> Passport
makePassport str =
  let kvs = splitOn " " str
      tuples = fmap makeKeyValue kvs
   in Passport $ Map.fromList tuples

mandatoryKeys :: [String]
mandatoryKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

validatePassport :: Passport -> Bool
validatePassport passport =
  all (flip Map.member $ insides passport) mandatoryKeys

countValidPassports :: [String] -> Int
countValidPassports rows =
  let passports = (fmap makePassport) $ splitChunks rows
   in length $ filter validatePassport passports

day4 :: IO ()
day4 = do
  contents <- readFile "input-day4.txt"
  putStrLn $ "Day 4: " ++ (show . countValidPassports $ lines contents)
