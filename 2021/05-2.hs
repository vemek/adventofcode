import System.IO
import Control.Monad
import Data.List
import Text.Printf
import qualified Data.IntMap as M

data Point = Point { x :: Int, y :: Int }
  deriving (Eq, Ord)
data Line = Line { start :: Point, end :: Point }

instance Show Point where
  show (Point x y) = printf "P[%d,%d]" x y

instance Show Line where
  show (Line (Point x1 y1) (Point x2 y2)) = printf "L[%d,%d -> %d,%d]" x1 y1 x2 y2

line x1 y1 x2 y2 = Line (Point x1 y1) (Point x2 y2)

pointsOnLine :: Line -> [Point]
pointsOnLine (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = map (Point x1) [minY..maxY]
  | y1 == y2 = map (flip Point y1) [minX..maxX]
  | otherwise = map (\(x, y) -> Point x y) $ zip xs ys
  where [minX,maxX] = sort [x1,x2]
        [minY,maxY] = sort [y1,y2]
        dxs = [minX..maxX]
        dys = [minY..maxY]
        xs = if x1 <= x2 then dxs else reverse dxs
        ys = if y1 <= y2 then dys else reverse dys

lineMapKey :: Int -> Int -> Int -> Int
lineMapKey x y maxX = x + y * (maxX + 1)

lineMap :: [Line] -> [[Int]]
lineMap lines = map row [0..maxY]
  where row y = map (col y) [0..maxX]
        col y x = case M.lookup (key x y) lmap of
                    Just n -> n
                    Nothing -> 0
        maxX = foldr1 max $ map x allPoints
        maxY = foldr1 max $ map y allPoints
        allPoints = concat $ map pointsOnLine lines
        key x y = lineMapKey x y maxX
        lmap = lineMap' lines

lineMap' :: [Line] -> M.IntMap Int
lineMap' lines = foldr addToMap M.empty points
  where addToMap p m = M.insert (key p) (newVal p m) m
        points = concat $ map pointsOnLine lines
        maxX = foldr1 max $ map x points
        key p = lineMapKey (x p) (y p) maxX
        newVal p m = case M.lookup (key p) m of
                   Just n -> n + 1
                   Nothing -> 1

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

howManyOverlaps :: M.IntMap Int -> Int
howManyOverlaps m = M.foldr (\n a -> if n > 1 then a + 1 else a) 0 m

-- As expected, filling in the IntMap _way_ faster.
main = do
  content <- readFile "05-input.txt"
  let lines = parseLines content
  --mapM putStrLn $ map (\x -> (show x) ++ ": " ++ (show $ pointsOnLine x)) lines
  --let lmap = lineMap lines
  let lmap' = lineMap' lines
  --putStrLn $ showLineMap lmap
  putStrLn $ "Total overlaps: " ++ (show $ howManyOverlaps lmap')
