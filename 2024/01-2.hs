import System.IO
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

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

buildScores :: [Int] -> IntMap Int
buildScores = buildScores' IntMap.empty

buildScores' :: IntMap Int -> [Int] -> IntMap Int
buildScores' scores [] = scores
buildScores' scores (x:xs) = buildScores' newScores xs
    where newScores = IntMap.insert x score scores
          score = oldScore + 1
          oldScore = IntMap.findWithDefault 0 x scores

similarityScore :: IntMap Int -> Int -> Int
similarityScore scores x = score * x
    where score = IntMap.findWithDefault 0 x scores

main = do
  numbers <- numbersFromFile "01-1-input.txt"
  let (left, right) = listsToTuples numbers
  let scores = buildScores right
  let similarityScores = map (similarityScore scores) left
  putStrLn . show $ sum similarityScores