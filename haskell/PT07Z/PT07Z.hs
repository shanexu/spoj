{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad(forM_, foldM)

problem :: [String] -> (Int, Array Int [Int])
problem (l:ls) = runST $ do
  let n = read l
      e =  map (map read . words) ls
  arr <- newArray (1, n) [] :: ST s (STArray s Int [Int])
  forM_ e $ \[a1, a2] -> (a2:) `fmap` readArray arr a1 >>= writeArray arr a1 >> (a1:) `fmap` readArray arr a2 >>= writeArray arr a2
  (,)n `fmap` freeze arr

solve :: (Int, Array Int [Int]) -> Int
solve (len, edges) = runST $ do
  arr1 <- newArray (1, len) (-1)
  writeArray arr1 1 0
  (mu, mp) <- longestPath arr1 [1] (1, 0)
  if mp == len - 1
    then return mp
    else
    do
      arr2 <- newArray (1, len) (-1)
      writeArray arr2 mu 0
      snd `fmap` longestPath arr2 [mu] (mu, 0)

  where
    longestPath :: STUArray s Int Int -> [Int] -> (Int, Int) -> ST s (Int, Int)
    longestPath _ [] mm = return mm
    longestPath arr (v:vs) mm@(mu, cm)
      | cm == len - 1 = return mm
      | otherwise = do
          sv <- readArray arr v
          let f (us, (n, m)) u = do
                su <- readArray arr u
                if su == -1
                  then
                  do
                    let su' = sv + 1
                    writeArray arr u su'
                    return (u:us, if su' > m then (u, su') else (n, m))
                  else return (us, (n, m))
          foldM f (vs, (mu, cm)) (edges ! v) >>= uncurry (longestPath arr)

main :: IO ()
main = interact $ show . solve . problem . lines
