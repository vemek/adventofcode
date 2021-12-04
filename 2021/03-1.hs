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
bitColumns = bitColumns' []

bitColumns' cols [] = cols
bitColumns' cols ([]:_) = cols
bitColumns' cols xs = bitColumns' (col:cols) remaining
  where (col, remaining) = bitColumn xs

bitColumn :: [[Int]] -> ([Int], [[Int]])
bitColumn []     = ([], [])
bitColumn ([]:_) = ([], [])
bitColumn ((x:xs):ys) = ((x:yres), xs:yleft)
  where (yres, yleft) = bitColumn ys

gammaRate :: [[Int]] -> [Int]
gammaRate [] = []
gammaRate (xs:xss) = val:(gammaRate xss)
  where val   = if s > half then 1 else 0
        s     = sum xs
        half  = length xs `div` 2

epsilonRate :: [Int] -> [Int]
epsilonRate [] = []
epsilonRate (x:xs) = val:(epsilonRate xs)
  where val = if x == 0 then 1 else 0

rateToInt :: [Int] -> Int
rateToInt = rateToInt' 0

rateToInt' _ [] = 0
rateToInt' n (x:xs) = (x * 2^n) + rateToInt' (n+1) xs

main = do
  content <- readFile "03-input.txt"
  let binStrs = lines content
  let gr = gammaRate $ bitColumns $ map bitString binStrs
  let er = epsilonRate gr
  let g = rateToInt gr
  let e = rateToInt er
  putStrLn $ "Gamma: " ++ (show g)
  putStrLn $ "Epsilon: " ++ (show e)
  putStrLn $ "Power rate: " ++ (show $ g * e)
