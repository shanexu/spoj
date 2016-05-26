import Data.Int (Int64)
import Data.Char (digitToInt)
import Control.Monad

type BigNumber = [Int64]

power :: Int64
powerLen :: Int64

power = 1000000000
powerLen = 9

split :: Int64 -> BigNumber
split 0 = []
split n = r : split q
  where (q, r) = n `quotRem` power

(|+|) :: BigNumber -> BigNumber -> BigNumber
[] |+| ys = ys
xs |+| [] = xs
(x:xs) |+| (y:ys) = case split (x+y) of
                      [] -> 0 : (xs |+| ys)
                      (z:zs) -> z : (zs |+| xs |+| ys)

(|-|) :: BigNumber -> BigNumber -> BigNumber
[] |-| [] = []
[] |-| ys = init ys ++ [- last ys]
xs |-| [] = xs
(x:xs) |-| (y:ys)
  | x == y = 0 : (xs |-| ys)
  | x < y = (power + x - y) : (xs |-| [1] |-| ys)
  | x > y = (x - y) : (xs |-| ys)

(|*|) :: BigNumber -> BigNumber -> BigNumber
[] |*| _ = []
_ |*| [] = []
(x:xs) |*| [y] = split (x * y) |+| (0 : (xs |*| [y]))
xs |*| (y:ys) = xs |*| [y] |+| (0 : (xs |*| ys))

stringToBigNumber :: String -> BigNumber
stringToBigNumber str = go (fromIntegral . length $ str) str []
  where go :: Int64 -> String -> BigNumber -> BigNumber
        go 0 _ num = num
        go len (x:xs) ns
          | len `mod` powerLen == 0 = go (len - 1) xs $ (fromIntegral . digitToInt $ x) : ns
          | otherwise = case ns of
                             [] -> go (len - 1) xs [fromIntegral . digitToInt $ x]
                             (t:ts) -> go (len - 1) xs $ ((fromIntegral . digitToInt $ x) + t * 10):ts

format :: BigNumber -> String
format n = dropWhile (=='0') . concat $ map (tail . show . (+power)) $ reverse n

julka :: BigNumber -> BigNumber -> BigNumber -> (String, String)
julka total diff diff0 = let klaudia0 = (total |+| diff) |*| [5]
                             natalia0 = klaudia0 |-| diff0
                         in (init . format $ klaudia0, init . format $ natalia0)

solveProblems :: [String] -> IO ()
solveProblems = foldM_ f ""
  where f "" totalStr = return totalStr
        f totalStr diffStr = let (klaudia, natalia) = julka (stringToBigNumber totalStr) (stringToBigNumber diffStr) (stringToBigNumber (diffStr ++ "0"))
                             in putStrLn klaudia >> putStrLn natalia >> return ""

main :: IO ()
main = getContents >>= solveProblems . lines
