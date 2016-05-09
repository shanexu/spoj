import Data.Int (Int64)
import Data.List (intercalate)

type BigNumber = [Int64]

power :: Int64
power = 1000000000

split :: Int64 -> BigNumber
split 0 = []
split n = r : split q
  where q = n `div` power
        r = n - power * q

format :: BigNumber -> String
format n = dropWhile (=='0') . concat $ map (tail . show . (+power)) $ reverse n

(|+|) :: BigNumber -> BigNumber -> BigNumber
[] |+| ys = ys
xs |+| [] = xs
(x:xs) |+| (y:ys) = case split (x+y) of
                        [] -> 0 : (xs |+| ys)
                        (z:zs) -> z : (zs |+| xs |+| ys)

(|*|) :: BigNumber -> BigNumber -> BigNumber
[] |*| _ = []
_ |*| [] = []
(x:xs) |*| [y] = split (x * y) |+| (0 : (xs |*| [y]))
xs |*| (y:ys) = xs |*| [y] |+| (0 : (xs |*| ys))

readInt64 :: String -> Int64
readInt64 = read

factorial :: Int64 -> BigNumber
factorial n = factorial' n [1]
  where
    factorial' 1 res = res
    factorial' num res = factorial' (num - 1) (split num |*| res)

main :: IO ()
main = putStrLn . join . map (format . factorial . readInt64) . tail . lines =<< getContents
  where join = intercalate "\n"

