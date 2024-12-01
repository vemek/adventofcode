import System.IO
import Data.List

numbersFromFile :: String -> IO [Int]
numbersFromFile path = do
  contents <- readFile path
  return $ map readInt . words $ contents

readInt :: String -> Int
readInt = read

listsToTuples :: [Int] -> ([Int], [Int])
listsToTuples xs = listsToTuples' xs ([], [])

listsToTuples' :: [Int] -> ([Int], [Int]) -> ([Int], [Int])
listsToTuples' [] tuples = tuples
listsToTuples' (a:b:xs) (as, bs) = listsToTuples' xs ((a:as), (b:bs))

sortedTuples :: ([Int], [Int]) -> ([Int], [Int])
sortedTuples (a, b) = (sort a, sort b)

absDiff :: (Int, Int) -> Int
absDiff (x, y) = abs $ x - y

main = do
  numbers <- numbersFromFile "01-1-input.txt"
  let tuples = listsToTuples numbers
  let diffs = map absDiff $ uncurry zip $ sortedTuples tuples
  putStrLn . show $ sum diffs
