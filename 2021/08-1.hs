import System.IO
import Control.Monad
import Data.List
import Text.Printf

data Segment = A | B | C | D | E | F | G
  deriving (Eq, Ord, Show)

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Ord, Show)

type Wiring = [Segment]

data Entry = Entry
  { signalPatterns  :: [Wiring],
    displayPatterns :: [Wiring] }

instance Show Entry where
  show e = signalStr ++ " | " ++ displayStr
    where signalStr   = foldr1 (\a b -> a ++ " " ++ b) $ map showWiring $ signalPatterns e
          displayStr  = foldr1 (\a b -> a ++ " " ++ b) $ map showWiring $ displayPatterns e

showWiring :: Wiring -> String
showWiring w = foldr1 (++) $ map show w

possibleDigits :: Wiring -> [Digit]
possibleDigits w = case length w of
                     2          -> [One]
                     3          -> [Seven]
                     4          -> [Four]
                     7          -> [Eight]
                     otherwise  -> [Zero, Two, Three, Five, Six, Nine]

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn d s = foldr f [""] s
  where f c l@(x:xs)
          | c == d = "":l
          | otherwise = (c:x):xs

parseWiring :: String -> Wiring
parseWiring "" = []
parseWiring (d:ds) = digit:(parseWiring ds)
  where digit = case d of
                       'a' -> A
                       'b' -> B
                       'c' -> C
                       'd' -> D
                       'e' -> E
                       'f' -> F
                       'g' -> G

parseEntry :: String -> Entry
parseEntry s = Entry signals displays
  where [signalStr, displayStr] = splitOn '|' s
        signals = map parseWiring $ words signalStr
        displays = map parseWiring $ words displayStr

uniqueSignal s = case possibleDigits s of
                   [One]      -> True
                   [Seven]    -> True
                   [Four]     -> True
                   [Eight]    -> True
                   otherwise  -> False

countUniquesDisplayed :: [Wiring] -> Int
countUniquesDisplayed [] = 0
countUniquesDisplayed (s:ss) = curValue + countUniquesDisplayed ss
  where curValue = if uniqueSignal s then 1 else 0

main = do
  content <- readFile "08-input.txt"
  let entries = map parseEntry $ lines content
  putStrLn $ "Total count of unique values in displays: " ++ (show $ sum $ map countUniquesDisplayed $ map displayPatterns entries)
