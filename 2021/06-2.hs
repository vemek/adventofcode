import System.IO
import Control.Monad
import Data.List
import Text.Printf

type Fish = Int

-- For a fish with daysTillSpawn n, figure out how many fish it spawns, plus how many they each spawn, recursively
howManySpawns :: Int -> Fish -> Int
howManySpawns numDays f = howManySpawns' numDays f 0

howManySpawns' :: Int -> Fish -> Int -> Int
howManySpawns' numDays f curDay
  | daysLeft < 0  = 1 -- count the fish this was called for once on the last day
  | otherwise = (howManySpawns' numDays f nextDay) + newSpawn
  where newSpawn = if newSpawnDay then howManySpawns' numDays (curDay + 8) curDay else 0
        newSpawnDay = curDay >= f && (curDay - f) `mod` 7 == 1
        daysLeft = numDays - curDay
        nextDay = curDay + 1

parseInput :: String -> [Fish]
parseInput s = map (read :: String -> Int) intStrs
  where intStrs = splitOn ',' s

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = foldr f [""] s
  where f c l@(x:xs)
          | c == d = "":l
          | otherwise = (c:x):xs

showFishStates :: [Fish] -> String
showFishStates fishes = foldr1 (\a b -> a ++ "," ++ b) $ map show fishes

{-
   Iterating over lots of timers is expensive in CPU and memory. Let's project forward from the initial state
   using modular arithmetic and some recursion instead.
-}
main = do
  content <- readFile "06-input-test.txt"
  let fishes = parseInput content
  --let total = sum $ map (howManySpawns 256) fishes
  --putStrLn $ "After 256 days: " ++ (show total)
  --putStrLn $ foldr1 (\a b -> a ++ "\n" ++ b) $ map (\(n, f) -> "After " ++ (show n) ++ " days: " ++ (show f)) $ map (\n -> (n, sum $ map (howManySpawns n) fishes)) [0..18]
  --putStrLn "First fish:"
  putStrLn $ foldr1 (\a b -> a ++ "\n" ++ b) $ map (\(n, f) -> "After " ++ (show n) ++ " days: " ++ (show f)) $ map (\n -> (n, howManySpawns n 3)) [0..18]
