import System.IO

numbersFromFile :: String -> IO [Int]
numbersFromFile path = do
  contents <- readFile path
  return $ map readInt . words $ contents

readInt :: String -> Int
readInt = read

pairAscending [a,b] = a < b

allPairs []  = []
allPairs [_] = []
allPairs (x:y:xs) = [x,y]:(allPairs (y:xs))

slidingWindow3 []     = []
slidingWindow3 [_]    = []
slidingWindow3 [_,_]  = []
slidingWindow3 (x:y:z:xs) = (x+y+z):(slidingWindow3 (y:z:xs))

main = do
  numbers <- numbersFromFile "01-1-input.txt"
  let increases = map pairAscending . allPairs . slidingWindow3 $ numbers
  putStrLn . show . sum $ map (\x -> if x then 1 else 0) increases
