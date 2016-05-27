{-# OPTIONS_GHC -optc-O2 #-}

import Data.List(foldl')
import Data.Int(Int64)
import Control.Monad(replicateM_, replicateM)

solve :: Int64 -> [Int64] -> Bool
solve n cs = foldl' (\r c -> (r + (c `rem` n)) `rem` n) 0 cs == 0

format :: Bool -> String
format True = "YES"
format False = "NO"

main :: IO ()
main = do
  t <- read `fmap` getLine
  replicateM_ t $ do
    _ <- getLine
    n <- read `fmap` getLine
    cs <- replicateM n $ read `fmap` getLine
    putStrLn . format $ solve (fromIntegral n) cs
