{-# OPTIONS_GHC -optc-O2 #-}

import Control.Monad

series :: [Int] -> [Int]
series (x:xs) = go xs (repeat x)
  where go [] s = s
        go (y:ys) s = let s1 = y : zipWith (+) (tail s) s1
                      in go ys s1

seeds :: [Int] -> [Int]
seeds xs = go xs []
  where
    go ys@(y:ys1) ss = if all (==y) ys1
                       then y : ss
                       else go (zipWith (-) ys1 ys) (last ys1 : ss)

cmpls :: Int -> [Int] -> [Int]
cmpls n = take n . tail . series . seeds

solveProblems :: [String] -> IO ()
solveProblems = foldM_ f 0
  where f 0 l = let [_, n] = map read . words $ l
                in return n
        f n l = putStrLn (unwords . map show $ cmpls n (map read . words $ l)) >> return 0

main :: IO ()
main = getContents >>= solveProblems . tail . lines
