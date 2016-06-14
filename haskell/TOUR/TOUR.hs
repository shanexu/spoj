{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad(forM_)
import Data.List(foldl')
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe

select :: a -> a -> Bool -> a
select a1 a2 b = if b then a1 else a2

check :: Array Int [Int] -> Int -> Bool
check edges no = runST $ do
  mem <- newArray (bounds edges) False :: ST s (STUArray s Int Bool)
  let m = snd . bounds $ edges
      go step [] = return step
      go step (u:us)
        | step == m = return step
        | otherwise = readArray mem u >>= select (go step us) (writeArray mem u True >> go (step+1) (foldl' (flip(:)) us (edges ! u)))
  (==m) `fmap` go 0 [no]

checkAll :: Array Int [Int] -> Int
checkAll edges = runST $ do
  let m = snd . bounds $ edges
  mem <- newArray ((1,1), (m,m)) False :: ST s (STUArray s (Int, Int) Bool)
  vis <- newArray (1,m) False :: ST s (STUArray s Int Bool)
  undefined

solve :: Array Int [Int] -> Int
solve edges = length $ filter (check edges) (indices edges)

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
  putStr . unlines $ foldl' (\rs p -> (show.solve $ p):rs) [] ps
