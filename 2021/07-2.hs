import System.IO
import Control.Monad
import Data.List
import Text.Printf

type CrabPos = Int

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = foldr f [""] s
  where f c l@(x:xs)
          | c == d = "":l
          | otherwise = (c:x):xs

distTo :: CrabPos -> CrabPos -> CrabPos
distTo newPos curPos = binomial . abs $ newPos - curPos

costToEachDistance :: [CrabPos] -> [Int]
costToEachDistance startingPositions = do
  distance <- [minH..maxH]
  return $ sum $ map (distTo distance) startingPositions
    where maxH = foldr1 max startingPositions
          minH = foldr1 min startingPositions

binomial n = n * (n + 1) `div` 2

main = do
  content <- readFile "07-input.txt"
  let crabPositions = map (read :: String -> CrabPos) $ splitOn ',' content
  --putStrLn $ foldr (\(a,b) s -> (show a) ++ ": " ++ (show b) ++ "\n" ++ s) "" $ zip [0..16] $ costToEachDistance crabPositions
  putStrLn $ "Cheapest position fuel use: " ++ (show $ foldr1 min $ costToEachDistance crabPositions)

