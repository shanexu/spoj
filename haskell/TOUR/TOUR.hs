{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad(forM_, foldM, unless)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe

solves :: [Array Int [Int]] -> [Int]
solves ps = runST $ do
  mem <- newArray ((1,1), (1000,1000)) False :: ST s (STUArray s (Int, Int) Bool)
  solves' ps mem []
  where solves' [] _ rs = return rs
        solves' (x:xs) mem rs = ((:rs) `fmap` solve x mem) >>= solves' xs mem

solve :: Array Int [Int] -> STUArray s (Int, Int) Bool -> ST s Int
solve edges mem = do
  let m = snd . bounds $ edges
  forM_ [(i, j) | i <- [1..m], j <-[1..m]] (\idx -> writeArray mem idx False)
  forM_ [1..m] (\u -> travel u [u] edges mem)
  foldM (\count u -> (\tour -> if tour then count + 1 else count) `fmap` checkOne u m mem) 0 [1..m]

checkOne :: Int -> Int -> STUArray s (Int, Int) Bool -> ST s Bool
checkOne _ 0 _ = return True
checkOne source target mem = readArray mem (source, target) >>= \hasPath -> if hasPath then checkOne source (target-1) mem else return False

travel :: Int -> [Int] -> Array Int [Int] -> STUArray s (Int, Int) Bool -> ST s ()
travel target previous edges mem = do
  previous' <- foldM (\ps p -> readArray mem (p, target) >>= \vd -> if vd then return ps else writeArray mem (p, target) True >> return (p:ps)) [] previous
  unless (null previous') $ forM_ (edges ! target) (\u -> travel u (target : previous') edges mem)

readInt :: L.ByteString -> Int
readInt = fst . fromJust . L.readInt

problems :: Int -> ([Array Int [Int]], L.ByteString) -> ([Array Int [Int]], L.ByteString)
problems 0 p = p
problems t (ps, ss) = let (l, ss') = L.break (=='\n') (L.tail ss)
                          n = readInt l
                      in problems (t-1) $ runST $
                         do
                           arr <- newArray (1,n) [] :: ST s (STArray s Int [Int])
                           let process i ls
                                 | i == (n+1) = return ls
                                 | otherwise = let (str, ls') = L.break(=='\n') (L.tail ls)
                                                   betters = tail . map readInt $ L.split ' ' str
                                               in forM_ betters (\b -> ((i:) `fmap` readArray arr b) >>= writeArray arr b) >> process (i+1) ls'
                           ss'' <- process 1 ss'
                           (flip (,) ss'' . (:ps)) `fmap` freeze arr
main :: IO ()
main = do
  ss <- L.getContents
  let (l, ss') = L.break (=='\n') ss
      ps = fst $ problems (readInt l) ([], ss')
  putStr . unlines $ map show $ solves ps
