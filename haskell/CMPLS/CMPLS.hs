{-# OPTIONS_GHC -optc-O2 #-}

import Control.Monad

series :: [Int] -> [Int]
series (x:xs) = go xs (repeat x)
  where go [] s = s
        go (y:ys) s = let s' = y : zipWith (+) (tail s) s'
                      in go ys s'

seeds :: [Int] -> [Int]
seeds xs = go xs []
  where
    go ys@(y:ys') ss = if all (==y) ys'
                       then y : ss
                       else go (zipWith (-) ys' ys) (last ys' : ss)

cmpls :: Int -> [Int] -> [Int]
cmpls n = take n . tail . series . seeds

solveProblems :: [String] -> IO ()
solveProblems = foldM_ f 0
  where f 0 l = let [_, n] = map read . words $ l
                in return n
        f n l = putStrLn (unwords . map show $ cmpls n (map read . words $ l)) >> return 0

main :: IO ()
main = getContents >>= solveProblems . tail . lines
