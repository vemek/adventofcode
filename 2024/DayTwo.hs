module DayTwo where

import Data.List
import System.IO
import Data.Map (valid)

numbersFromFile :: String -> IO [[Int]]
numbersFromFile path = do
  contents <- readFile path
  let reportStrings = map words $ lines contents
  let reports = map (map readInt) reportStrings
  return reports

readInt :: String -> Int
readInt = read

allPairs :: (Int -> Int -> Bool) -> [(Int, Int)] -> Bool
allPairs f = all $ uncurry f

validStepping :: Int -> Int -> Bool
validStepping x y = diff >= 1 && diff <= 3
    where diff = abs $ x - y

safeReport :: [Int] -> Bool
safeReport xs = validDirection && validSteppings
  where pairs = zip (init xs) (tail xs)
        validDirection = allPairs (<=) pairs || allPairs (>) pairs
        validSteppings = allPairs validStepping pairs

reportPerms :: [Int] -> [[Int]]
reportPerms xs = xs : reportPerms' [] xs

reportPerms' :: [Int] -> [Int] -> [[Int]]
reportPerms' _ [] = []
reportPerms' xs (y:ys) = chopOffRightHead:nextPerm
    where chopOffRightHead = xs ++ ys
          nextPerm = reportPerms' (xs ++ [y]) ys

anySafeReport :: [Int] -> Bool
anySafeReport xs = any safeReport perms
    where perms = reportPerms xs

runDay :: String -> (String, String)
runDay input = do
  let reports = map (map readInt . words) $ lines input
  let resultOne = show $ length . filter id $ map safeReport reports
  let resultTwo = show $ length . filter id $ map anySafeReport reports
  (resultOne, resultTwo)
