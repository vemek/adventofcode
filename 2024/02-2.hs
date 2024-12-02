import Data.List
import System.IO

numbersFromFile :: String -> IO [[Int]]
numbersFromFile path = do
  contents <- readFile path
  let reportStrings = map words $ lines contents
  let reports = map (map readInt) reportStrings
  return reports

readInt :: String -> Int
readInt = read

reportPerms :: [Int] -> [[Int]]
reportPerms xs = xs : reportPerms' [] xs

reportPerms' :: [Int] -> [Int] -> [[Int]]
reportPerms' _ [] = []
reportPerms' xs (y:ys) = chopOffRightHead:nextPerm
    where chopOffRightHead = xs ++ ys
          nextPerm = reportPerms' (xs ++ [y]) ys

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

anySafeReport :: [Int] -> Bool
anySafeReport xs = any safeReport perms
    where perms = reportPerms xs

main = do
  reports <- numbersFromFile "02-1-input.txt"
  print $ length.filter id $ map anySafeReport reports
