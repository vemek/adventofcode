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
distTo newPos curPos = abs $ newPos - curPos

costToEachDistance :: [CrabPos] -> [Int]
costToEachDistance startingPositions = do
  distance <- [minH..maxH]
  return $ sum $ map (distTo distance) startingPositions
    where maxH = foldr1 max startingPositions
          minH = foldr1 min startingPositions

main = do
  content <- readFile "07-input.txt"
  let crabPositions = map (read :: String -> CrabPos) $ splitOn ',' content
  putStrLn $ "Cheapest position fuel use: " ++ (show $ foldr1 min $ costToEachDistance crabPositions)

