{-# OPTIONS_GHC -optc-O2 #-}

import Control.Monad

series :: [Int] -> [Int]
series [] = repeat 0
series (x:xs) = let s = x : zipWith (+) (tail (series xs)) s in s

seeds :: [Int] -> [Int]
seeds [] = []
seeds xs = last xs : seeds (zipWith (-) (tail xs) xs)

cmpls :: Int -> [Int] -> [Int]
cmpls n = take n . tail . series . seeds

solveProblems :: [String] -> IO ()
solveProblems = foldM_ f 0
  where f 0 l = let [_, n] = map read . words $ l
                in return n
        f n l = putStrLn (unwords . map show $ cmpls n (map read . words $ l)) >> return 0

main :: IO ()
main = getContents >>= solveProblems . tail . lines
