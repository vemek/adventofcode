import System.IO
import Control.Monad
import Data.List
import Text.Printf

data Board      = Board [Int]
data BoardState = BoardState Board [Int] -- represents a board and the called numbers

instance Show Board where
  show b = foldl (++)  "" $  map showRow $ rows b
    where showRow (x1:x2:x3:x4:x5:[]) = printf "%3d %3d %3d %3d %3d\n" x1 x2 x3 x4 x5

instance Show BoardState where
  show (BoardState b ys) = foldl (++)  "" $  map showRow $ rows b
    where showRow (x1:x2:x3:x4:x5:[]) = printf "%3s %3s %3s %3s %3s\n" (sym x1) (sym x2) (sym x3) (sym x4) (sym x5)
          sym x = if x `elem` ys then ("X" ++ show x) else show x

board :: BoardState -> Board
board (BoardState b _) = b

-- check if a board wins
wins :: BoardState -> Bool
wins (BoardState b ys) = any allCalled bingoLines
  where bingoLines = rows b ++ cols b
        allCalled line = all (flip elem ys) line

-- Get rows of a board as a list
rows (Board s) = rows' s
rows' [] = []
rows' xs = (take 5 xs):(rows' $ drop 5 xs)

-- Get cols of a board as a list
-- builds up five lists at a time, adding one item per row to each
cols :: Board -> [[Int]]
cols (Board s) = cols' s
cols' [] = [[],[],[],[],[]]
cols' (x1:x2:x3:x4:x5:xs) = [x1:x1s, x2:x2s, x3:x3s, x4:x4s, x5:x5s]
  where (x1s:x2s:x3s:x4s:x5s:[]) = cols' xs

-- Parse input and return selections and boards
parseInput :: String -> ([Int], [Board])
parseInput s = (selections $ head l, boards $ tail l)
  where l = lines s
        selections line = map (read :: String -> Int) $ splitOn ',' line
        boards      [] = []
        boards ("":xs) = boards xs
        boards      xs = (stringsToBoard $ take 5 xs):(boards $ drop 5 xs)

stringsToBoard :: [String] -> Board
stringsToBoard s = Board ints
  where nums = foldr (++) [] $ map words s
        ints = map (read :: String -> Int) nums

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = foldr f [""] s
  where f c l@(x:xs)
          | c == d = "":l
          | otherwise = (c:x):xs

score :: BoardState -> Int
score (BoardState (Board s) l@(y:ys)) = y * score' s
  where score'     [] = 0
        score' (x:xs) = scoreFor x + score' xs
        scoreFor x = if x `elem` l then 0 else x

-- Announce a new number and update board state
announce :: Int -> BoardState -> BoardState
announce n (BoardState b ys) = BoardState b (n:ys)

findLastWinner :: [BoardState] -> Maybe BoardState
findLastWinner boardStates = if length nonWinners == 1 then Just $ head nonWinners else Nothing
  where nonWinners = filter (not . wins) boardStates

lastWinner :: [Int] -> [BoardState] -> BoardState
lastWinner (x:xs) boardStates = case findLastWinner newBoardStates of
                              Just bs -> announce (head xs) bs -- hacky - this board will win on the *next* turn
                              Nothing -> (lastWinner xs newBoardStates)
                            where newBoardStates = map (announce x) boardStates

main = do
  content <- readFile "04-input.txt"
  let (selections, boards) = parseInput content
  let boardStates = map (\b -> BoardState b []) boards
  let allWins = map (\b -> BoardState b selections) boards
  putStrLn $ "Do all boards win eventually? " ++ (show $ all (\bs -> wins bs) allWins)
  putStrLn "If not, we need to maintain more state to determine the last winner."
  let winningBoard = lastWinner selections boardStates
  putStrLn $ "Last winning board:\n\n" ++ (show $ winningBoard)
  putStrLn $ "Score: " ++ (show $ score winningBoard)

