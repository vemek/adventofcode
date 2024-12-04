module DayThree where

import Text.Regex.TDFA

readInt :: String -> Int
readInt = read

mulNums :: String -> [Int]
mulNums = mulNums' . nextMatch

mulNums' :: Maybe (String, Int, Int) -> [Int]
mulNums' Nothing = []
mulNums' (Just (remaining, lhs, rhs)) = lhs * rhs : (mulNums' (nextMatch remaining))

nextMatch :: String -> Maybe (String, Int, Int)
nextMatch input = nextMatch' remaining matches
    where (_, _, remaining, matches) = input =~ "mul\\(([0-9]+),([0-9]+)\\)" :: (String, String, String, [String])

nextMatch' :: String -> [String] -> Maybe (String, Int, Int)
nextMatch' remaining [lhsStr, rhsStr] = Just (remaining, lhs, rhs)
    where lhs = readInt lhsStr
          rhs = readInt rhsStr
nextMatch' _ _ = Nothing

runDay :: String -> (String, String)
runDay input = do
  let resultOne = show $ sum $ mulNums input
  let resultTwo = ""
  (resultOne, resultTwo)
