import System.IO
import Control.Monad
import Data.List
import Text.Printf

data Point = Point { x :: Int, y :: Int }
  deriving Show

data Line = Line { start :: Point, end :: Point }
  deriving Show

line x1 y1 x2 y2 = Line (Point x1 y1) (Point x2 y2)

-- Is the given point on a line? Doesn't work for diagonals
pointOnLine :: Point -> Line -> Bool
pointOnLine (Point x y) (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = x == x1 && ((y1 <= y && y <= y2) || (y2 <= y && y <= y1))
  | y1 == y2 = y == y1 && ((x1 <= x && x <= x2) || (x2 <= x && x <= x1))
  | otherwise = False

lineMap :: [Line] -> [[Int]]
lineMap lines = map row [0..maxY]
  where row y = map (col y) [0..maxX]
        col y x = sum . (map fromEnum) $ map (pointOnLine (Point x y))  lines
        maxX = foldr1 max $ map x linePoints
        maxY = foldr1 max $ map y linePoints
        linePoints = (foldr (\l points -> (start l):(end l):points) [] lines)

showLineMap :: [[Int]] -> String
showLineMap lines = foldr (\a b -> a ++ "\n" ++ b) "\n" $ map showRow lines
  where showRow line = foldr1 (++) $ map sym line
        sym n = if n == 0 then "." else show n

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = foldr f [""] s
  where f c l@(x:xs)
          | c == d = "":l
          | otherwise = (c:x):xs

parseLines :: String -> [Line]
parseLines lineStr = map parseLine $ lines lineStr
parseLine line = Line (Point x1 y1) (Point x2 y2)
  where firstPointStr = head s
        secondPointStr = last s
        s = words line
        [x1, y1] = map (read :: String -> Int) $ splitOn ',' firstPointStr
        [x2, y2] = map (read :: String -> Int) $ splitOn ',' secondPointStr

howManyOverlaps :: [[Int]] -> Int
howManyOverlaps lines = sum . (map fromEnum) $ map (>1) $ concat lines

{-
   This is a bit slow - roughly O(maxX * MaxY * line count) I think. This could be made much faster
   by "drawing" the lines into a dictionary. Let's do that for part 2!
-}
main = do
  content <- readFile "05-input.txt"
  let lines = parseLines content
  let lmap = lineMap lines
  --putStrLn $ showLineMap lmap
  putStrLn $ "Total overlaps: " ++ (show $ howManyOverlaps lmap)
