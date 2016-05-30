{-# OPTIONS_GHC -optc-O2 #-}

import qualified Data.Set as S

solve :: Int -> Int
solve n = go n 0 S.empty
  where go num step hist
          | num == 1 = step
          | otherwise = if num `S.member` hist
                        then -1
                        else go (digitSquareSum num 0) (step+1) (S.insert num hist)

digitSquareSum :: Int -> Int -> Int
digitSquareSum 0 s = s
digitSquareSum num s = let (q, r) = num `quotRem` 10
                       in digitSquareSum q (s + r * r)

format :: Int -> String
format = show

problem :: String -> Int
problem = read

main :: IO ()
main = getContents >>= putStrLn . format . solve . problem . head . lines
