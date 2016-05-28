{-# OPTIONS_GHC -optc-O2 #-}

import Control.Monad(when, replicateM)

solve :: Int -> [Int] -> Int
solve n cs = let s = sum cs
             in if s `rem` n == 0
                then let a = s `div` n
                     in (`div` 2) . sum . map (abs.(a-)) $ cs
                else -1

process :: IO ()
process = do
  n <- read `fmap` getLine
  when(n /= -1) $ do
    cs <- replicateM n $ read `fmap` getLine
    print $ solve n cs
    process

main :: IO ()
main = process
