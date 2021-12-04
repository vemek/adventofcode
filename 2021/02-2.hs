import System.IO
import Control.Monad

data Instruction = Forward Int | Down Int | Up Int

-- Evaluate a list of instructions and return position, starting at [0,0]
eval :: [Instruction] -> (Int, Int)
eval = eval' 0 0 0

eval' :: Int -> Int -> Int-> [Instruction] -> (Int, Int)
eval' h d a [] = (h, d)
eval' h d a (x:xs) = case x of
                       Forward i -> eval' (h+i) (d + (a * i)) a xs
                       Down i -> eval' h d (a+i) xs
                       Up i -> eval' h d (a-i) xs

readInt :: String -> Int
readInt = read

parse :: String -> Maybe Instruction
parse line = case s of
              "forward" -> Just $ Forward i
              "down" -> Just $ Down i
              "up" -> Just $ Up i
              otherwise -> Nothing
             where (s:x:[]) = words line
                   i = readInt x

main = do
  contents <- readFile "02-input.txt"
  let instructionStr = lines contents
  let finalPos = fmap eval $ sequence $ map parse instructionStr
  putStrLn $ show $ fmap (\(x,y) -> x * y) finalPos
