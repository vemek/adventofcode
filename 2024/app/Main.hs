import System.Environment
import System.Exit

import qualified DayOne
import qualified DayTwo
import qualified DayThree

type DayFunc = String -> (String, String)

solution :: [String] -> String -> IO ()
solution [x] input = do
  let (resultOne, resultTwo) = pickDay x input
  putStrLn $ "Day " ++ x
  putStrLn $ "Part one: " ++ resultOne
  putStrLn $ "Part two: " ++ resultTwo
solution _ _ = die "Usage: aoc <day>, input on stdin"

pickDay :: String -> DayFunc
pickDay "1" = DayOne.runDay
pickDay "2" = DayTwo.runDay
pickDay "3" = DayThree.runDay
pickDay _     = const ("???", "???")

main :: IO ()
main = do
  args <- getArgs
  input <- getContents
  solution args input
