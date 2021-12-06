import System.IO
import Control.Monad
import Data.List
import Text.Printf

data Fish = Fish { daysTillSpawn :: Int }
  deriving Show

tick :: Fish -> Fish
tick (Fish n) = Fish m
  where m = if n > 0 then n - 1 else 6

-- list concatenation here is inefficient
step :: [Fish] -> [Fish]
step fishes = olderFishes ++ newFishes
  where olderFishes = map tick fishes
        newFishes = replicate numSpawners (Fish 8)
        numSpawners = sum $ map (fromEnum . (\(Fish n) -> n == 0)) fishes

nStepsLog :: Int -> [Fish] -> [(Int,[Fish])]
nStepsLog 0 fishes = []
nStepsLog n fishes = (n, olderFishes):(nStepsLog (n-1) olderFishes)
  where olderFishes = step fishes

howManyFishAfter :: Int -> [Fish] -> Int
howManyFishAfter 0 fishes = length fishes
howManyFishAfter n fishes = howManyFishAfter (n-1) olderFishes
  where olderFishes = step fishes

parseInput :: String -> [Fish]
parseInput s = map (Fish . (read :: String -> Int)) intStrs
  where intStrs = splitOn ',' s

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = foldr f [""] s
  where f c l@(x:xs)
          | c == d = "":l
          | otherwise = (c:x):xs

showFishStates :: [Fish] -> String
showFishStates fishes = foldr1 (\a b -> a ++ "," ++ b) $ map (show . daysTillSpawn) fishes

main = do
  content <- readFile "06-input.txt"
  let fishes = parseInput content
  --putStrLn $ "Initial state: " ++ (showFishStates fishes)
  --putStrLn $ foldr1 (\a b -> a ++ "\n" ++ b) $ map (\(d, f) -> "After " ++ (show (19 - d)) ++ " days: " ++ (showFishStates f)) $ nStepsLog 18 fishes
  putStrLn $ (show $ howManyFishAfter 80 fishes) ++ " fish after 80 days"
