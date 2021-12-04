import System.IO
import Control.Monad
import Data.Bits


-- Not super useful for this question but why not
parseBin :: String -> Int
parseBin s = parseBin' 0 $ reverse s

parseBin' _ [] = 0
parseBin' n (x:xs) = (val * 2 ^ n) + parseBin' (n+1) xs
  where val = if x == '0' then 0 else 1

bitString :: String -> [Int]
bitString [] = []
bitString (x:xs) = val:(bitString xs)
  where val = if x == '0' then 0 else 1

bitColumns :: [[Int]] -> [[Int]]
bitColumns = reverse . bitColumns' []

bitColumns' cols [] = cols
bitColumns' cols ([]:_) = cols
bitColumns' cols xs = bitColumns' (col:cols) remaining
  where (col, remaining) = bitColumn xs

bitColumn :: [[Int]] -> ([Int], [[Int]])
bitColumn []     = ([], [])
bitColumn ([]:_) = ([], [])
bitColumn ((x:xs):ys) = ((x:yres), xs:yleft)
  where (yres, yleft) = bitColumn ys

oxyFilter :: [Int] -> Int
oxyFilter [] = 1
oxyFilter xs = if pass then 1 else 0
  where pass = 2 * (sum xs) >= length xs

carFilter xs = if oxyFilter xs == 0 then 1 else 0

bitFilter :: ([Int] -> Int) -> [[Int]] -> [Int]
bitFilter _ [] = []
bitFilter filterVal l@(xs:xss) = head $ bitFilter' filterVal (length xs) 0 l

bitFilter' :: ([Int] -> Int) -> Int -> Int -> [[Int]] -> [[Int]]
bitFilter' filterVal c i xs
  | length xs == 1 = xs
  | i < c = bitFilter' filterVal c (i+1) $ bitFilterFindVal filterVal i xs
  | otherwise = xs

bitFilterFindVal :: ([Int] -> Int) -> Int -> [[Int]] -> [[Int]]
bitFilterFindVal _ _ [] = []
bitFilterFindVal filterVal i xs = bitFilterOnePass v i xs
  where v = filterVal col
        col = bitColumns xs !! i

bitFilterOnePass :: Int -> Int -> [[Int]] -> [[Int]]
bitFilterOnePass _ _ [] = []
bitFilterOnePass v i (x:xs)
  | v == n = x:(bitFilterOnePass v i xs)
  | otherwise = bitFilterOnePass v i xs
  where n = x !! i

rateToInt :: [Int] -> Int
rateToInt xs = rateToInt' 0 $ reverse xs

rateToInt' _ [] = 0
rateToInt' n (x:xs) = (x * 2^n) + rateToInt' (n+1) xs

main = do
  content <- readFile "03-input.txt"
  let binStrs = lines content
  let bits = map bitString binStrs
  let or = bitFilter oxyFilter bits
  let cr = bitFilter carFilter bits
  let o = rateToInt or
  let c = rateToInt cr
  putStrLn $ "O2: " ++ (show or) ++ " / " ++ (show o)
  putStrLn $ "CO2: " ++ (show cr) ++ " / " ++ (show c)
  putStrLn $ "Life support: " ++ (show $ o * c)
