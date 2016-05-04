import Data.Int (Int64)

type BigNumber = [Int64]

power :: Int64
power = 1000000000

split :: Int64 -> BigNumber
split 0 = []
split n = r:(split q)
  where q = n `div` power
        r = n - power * q

format :: BigNumber -> String
format n = (dropWhile (=='0')) . concat $ map (tail . show . (+power)) $ reverse n

add :: BigNumber -> BigNumber -> BigNumber
[] `add` ys = ys
xs `add` [] = xs
(x:xs) `add` (y:ys) = case split (x+y) of
                        [] -> 0 : (xs `add` ys)
                        (z:zs) -> z : (zs `add` xs `add` ys)

mul :: BigNumber -> BigNumber -> BigNumber
[] `mul` _ = []
_ `mul` [] = []
(x:xs) `mul` [y] = (split (x * y)) `add` (0:(xs `mul` [y]))
xs `mul` (y:ys) = xs `mul` [y] `add` (0:(xs `mul` ys))


readInt64 :: String -> Int64
readInt64 = read

factorial :: Int64 -> BigNumber
factorial 0 = [1]
factorial n = (split n) `mul` factorial (n-1)

main :: IO ()
main = mapM_ (putStrLn . format . factorial . readInt64) . tail . lines =<< getContents

