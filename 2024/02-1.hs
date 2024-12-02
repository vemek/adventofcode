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
allPairs f pairs = foldl1' (&&) $ map (uncurry f) pairs

validStepping :: Int -> Int -> Bool
validStepping x y = diff >= 1 && diff <= 3
    where diff = abs $ x - y

safeReport :: [Int] -> Bool
safeReport xs = validDirection && validSteppings
  where pairs = zip (init xs) (tail xs)
        validDirection = allPairs (<=) pairs || allPairs (>) pairs
        validSteppings = allPairs validStepping pairs

main = do
  reports <- numbersFromFile "02-1-input.txt"
  print $ length . filter id $ map safeReport reports
