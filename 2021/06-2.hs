import System.IO
import Control.Monad
import Data.List
import Text.Printf

type Fish = Int

parseInput :: String -> [Fish]
parseInput s = map (read :: String -> Int) intStrs
  where intStrs = splitOn ',' s

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = foldr f [""] s
  where f c l@(x:xs)
          | c == d = "":l
          | otherwise = (c:x):xs

apply :: Int -> (a -> a) -> (a -> a)
apply 0 _ = id
apply n f = (apply (n - 1) f) . f

-- move each fish into the next bucket, add spawns to the x8 bucket
step :: [Int] -> [Int]
step (x0:x1:x2:x3:x4:x5:x6:x7:x8:[]) = x1:x2:x3:x4:x5:x6:(x7+x0):x8:x0:[]

-- Count fish by days till spawn and bucket them
initBuckets :: [Fish] -> [Int]
initBuckets fishes = do
  daystillSpawn <- [0..8]
  return $ length $ filter (== daystillSpawn) fishes

{-
   Iterating over lots of timers is expensive in CPU and memory. Instead, let's keep
   buckets for each number of days till spawn.
-}
main = do
  content <- readFile "06-input.txt"
  let fishes = parseInput content
  let startBuckets = initBuckets fishes
  putStrLn $ "After 256 days: " ++ (show $ sum $ apply 256 step startBuckets)
