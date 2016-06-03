{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Data.Char(ord)
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Data.Maybe(fromJust)
import Control.Monad(foldM)

type Coordinate = (Int, Int)

str2Coordinate :: String -> Coordinate
str2Coordinate [x, y] = (ord x - ord 'a' + 1, ord y - ord '0')

l2t :: [a] -> (a, a)
l2t [x1, x2] = (x1, x2)

problem :: String -> (Coordinate, Coordinate)
problem = l2t . map str2Coordinate . words

(|+|) :: Coordinate -> Coordinate -> Coordinate
(x1, y1) |+| (x2, y2) = (x1+x2, y1+y2)

directions :: [Coordinate]
directions = [(1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1)]

solve :: (Coordinate, Coordinate) -> Int
solve (source, target)
  | source == target = 0
  | otherwise = runST $ do
      arr <- newArray ((1,1), (8,8)) (-1)
      writeArray arr source 0
      search directions [source] arr
  where
    search :: [Coordinate] -> [Coordinate] -> STUArray s Coordinate Int -> ST s Int
    search [] (_:vs) arr = search directions vs arr
    search (d:ds) vs@(v:_) arr = do
      step <- readArray arr v
      let u@(x,y) = v |+| d
      if u == target
        then return (step+1)
        else if x >=1 && y >= 1 && x <= 8 && y <= 8
             then
               do
                 st <- readArray arr u
                 if st == -1
                   then writeArray arr u (step+1) >> search ds (vs ++ [u]) arr
                   else search ds vs arr
             else search ds vs arr


main :: IO ()
main = interact $ unlines . map (show . solve . problem) . tail . lines
