import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf

data Segment = A | B | C | D | E | F | G | U
  deriving (Eq, Ord, Show)

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Ord, Show)

{-
   aaaa
  b    c
  b    c
   dddd
  e    f
  e    f
   gggg

   WiringSolution = (a b c d e f g)
-}
type Wiring = [Segment]
type WiringSolution = (Segment, Segment, Segment, Segment, Segment, Segment, Segment)

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

-- For a given wiring and digit, permutations across the possible solutions that would result in
wiringSolutions :: Digit -> Wiring -> [WiringSolution]
wiringSolutions Zero w
  | length w == 6 = map (mkSolution Zero) $ permutations w
  | otherwise = []
wiringSolutions One w
  | length w == 2 = map (mkSolution One) $ permutations w
  | otherwise = []
wiringSolutions Two w
  | length w == 5 = map (mkSolution Two) $ permutations w
  | otherwise = []
wiringSolutions Three w
  | length w == 5 = map (mkSolution Three) $ permutations w
  | otherwise = []
wiringSolutions Four w
  | length w == 4 = map (mkSolution Four) $ permutations w
  | otherwise = []
wiringSolutions Five w
  | length w == 5 = map (mkSolution Five) $ permutations w
  | otherwise = []
wiringSolutions Six w
  | length w == 6 = map (mkSolution Six) $ permutations w
  | otherwise = []
wiringSolutions Seven w
  | length w == 3 = map (mkSolution Seven) $ permutations w
  | otherwise = []
wiringSolutions Eight w
  | length w == 7 = map (mkSolution Eight) $ permutations w
  | otherwise = []
wiringSolutions Nine w
  | length w == 6 = map (mkSolution Nine) $ permutations w
  | otherwise = []

allPossibleWiringSolutions :: Wiring -> [WiringSolution]
allPossibleWiringSolutions w = do
  digit <- [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine]
  solution <- wiringSolutions digit w
  return $ solution

-- Apply a given wiring to a solution, filling in unknowns
mkSolution :: Digit -> Wiring -> WiringSolution
mkSolution Zero   [a,b,c,e,f,g]   = (a, b, c, U, e, f, g)
mkSolution One    [c,f]           = (U, U, c, U, U, f, U)
mkSolution Two    [a,c,d,e,g]     = (a, U, c, d, e, U, g)
mkSolution Three  [a,c,d,f,g]     = (a, U, c, d, U, f, g)
mkSolution Four   [b,c,d,f]       = (U, b, c, d, U, f, U)
mkSolution Five   [a,b,d,f,g]     = (a, b, U, d, U, f, g)
mkSolution Six    [a,b,d,e,f,g]   = (a, b, U, d, e, f, g)
mkSolution Seven  [a,c,f]         = (a, U, c, U, U, f, U)
mkSolution Eight  [a,b,c,d,e,f,g] = (a, b, c, d, e, f, g)
mkSolution Nine   [a,b,c,d,f,g]   = (a, b, c, d, U, f, g)

combineSolutions :: WiringSolution -> WiringSolution -> Maybe WiringSolution
combineSolutions (a1, b1, c1, d1, e1 ,f1, g1) (a2, b2, c2, d2, e2, f2, g2) = do
  a <- combineSegments a1 a2
  b <- combineSegments b1 b2
  c <- combineSegments c1 c2
  d <- combineSegments d1 d2
  e <- combineSegments e1 e2
  f <- combineSegments f1 f2
  g <- combineSegments g1 g2
  return $ (a, b, c, d, e, f, g)

combineSegments :: Segment -> Segment -> Maybe Segment
combineSegments U d           = Just d
combineSegments d U           = Just d
combineSegments x y | x == y  = Just x
combineSegments _ _           = Nothing

findValidSolutions :: [Wiring] -> [WiringSolution]
findValidSolutions ws = findValidSolutions' ws [(U,U,U,U,U,U,U)]

findValidSolutions' :: [Wiring] -> [WiringSolution] -> [WiringSolution]
findValidSolutions' [] solutions = solutions
findValidSolutions' (w:ws) solutions = findValidSolutions' ws newSolutions
  where newSolutions = catMaybes $ findValidSolutionsForWiring w solutions

-- Take an existing partial solution and apply the next step
findValidSolutionsForWiring :: Wiring -> [WiringSolution] -> [Maybe WiringSolution]
findValidSolutionsForWiring w ss = do
  s1 <- ss
  s2 <- allPossibleWiringSolutions w
  return $ combineSolutions s1 s2

main = do
  --content <- readFile "08-input-test.txt"
  let content = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  --let entries = map parseEntry $ lines content
  let entry = parseEntry content
  putStrLn $ show $ findValidSolutions $ signalPatterns entry
