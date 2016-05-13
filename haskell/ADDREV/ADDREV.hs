import Data.Char(digitToInt, intToDigit)
import Data.List(foldl1')

split :: Int -> [Int]
split 0 = []
split n = r : split q
  where q = n `div` 10
        r = n - 10 * q

(|+|) :: String -> String -> String
[] |+| ys = ys
xs |+| [] = xs
(x:xs) |+| (y:ys) = case map intToDigit . split $ (digitToInt x + digitToInt y) of
                      [] -> '0' : (xs |+| ys)
                      (z:zs) -> z : (zs |+| xs |+| ys)

main :: IO ()
main = getContents >>= putStr . unlines . map (dropWhile (=='0') . foldl1' (|+|) . words) . tail . lines
