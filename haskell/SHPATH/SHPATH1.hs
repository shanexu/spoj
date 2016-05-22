{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Control.Monad (foldM, liftM, ap, when, forM_, liftM2, liftM)
import qualified Data.Map as M

data Heap s = Heap { _capacity :: Int
                   , _size :: Int
                   , _prios :: STUArray s Int Int
                   , _vals :: STUArray s Int Int
                   , _indices :: STUArray s Int Int
                   }

struct :: Heap s -> ST s ([(Int, Int)], [(Int, Int)])
struct (Heap c s ps vs is) =
  (,) `liftM` foldM (\r k -> (\p v -> r ++ [(p, v)]) `liftM` readArray ps k
                                                        `ap` readArray vs k) [] [1 .. s]
         `ap` foldM (\r k -> (\i -> r ++ [(k, i)]) `fmap` readArray is k) [] [1 .. c]

fromList :: [(Int, Int)] -> ST s (Heap s)
fromList pvs = do
  let capacity = length pvs
  ps <- newArray (1, capacity) maxBound
  vs <- newArray (1, capacity) maxBound
  is <- newArray (1, capacity) maxBound
  foldM (\h (p, v) -> insert p v h) (Heap capacity 0 ps vs is) pvs

insert :: Int -> Int -> Heap s -> ST s (Heap s)
insert p v (Heap c s ps vs is) = do
  let s' = s + 1
  swim p v s' ps vs is
  return (Heap c s' ps vs is)

deleteMin :: Heap s -> ST s ((Int, Int), Heap s)
deleteMin (Heap c s ps vs is) = do
  let s' = s - 1
  p1 <- readArray ps 1
  v1 <- readArray vs 1
  p <- readArray ps s
  v <- readArray vs s
  sink p v 1 s' ps vs is
  return ((p1, v1), Heap c s' ps vs is)

increaseTo :: Int -> Int -> Heap s -> ST s ()
increaseTo p v (Heap _ s ps vs is) = readArray is v >>= \i -> sink p v i s ps vs is

decreaseTo :: Int -> Int -> Heap s -> ST s ()
decreaseTo p v (Heap _ s ps vs is) = readArray is v >>= \i -> swim p v i ps vs is

sink :: Int -> Int -> Int -> Int -> STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int -> ST s ()
sink p v i n ps vs is
  | i * 2 > n = writeArray ps i p >> writeArray vs i v >> writeArray is v i
  | i * 2 == n = let j = i * 2
                 in readArray ps j >>=
                    \pj ->
                      if p > pj
                      then
                        readArray vs j >>=
                        \vj ->
                          writeArray ps i pj >> writeArray vs i vj >> writeArray is vj i >> sink p v j n ps vs is
                      else
                        writeArray ps i p >> writeArray vs i v >> writeArray is v i

  | otherwise = let l = i * 2
                    r = l + 1
                in readArray ps l >>=
                   \pl ->
                     readArray ps r >>=
                     \pr ->
                       let (j, pj) = if pl < pr then (l, pl) else (r, pr)
                       in if p > pj
                          then
                            readArray vs j >>=
                            \vj ->
                              writeArray ps i pj >> writeArray vs i vj >> writeArray is vj i >> sink p v j n ps vs is
                          else
                            writeArray ps i p >> writeArray vs i v >> writeArray is v i

swim :: Int -> Int  -> Int -> STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int -> ST s ()
swim p v s ps vs is
  | s == 1 = writeArray ps s p >> writeArray vs s v >> writeArray is v s
  | otherwise = let s' = s `div` 2
                in readArray ps s' >>=
                   \p' ->
                     if p < p'
                     then
                       readArray vs s' >>=
                       \v' ->
                         writeArray ps s p' >> writeArray vs s v' >> writeArray is v' s >> swim p v s' ps vs is
                     else
                       writeArray ps s p >> writeArray vs s v >> writeArray is v s

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls

dijkstra ::  Int -> Int -> Array Int [(Int,Int)] -> Int
dijkstra source target cities = runST $ do
  minDist <- newArray b maxBound :: ST s (STUArray s Int Int)
  writeArray minDist source 0
  heap <- fromList (zip (replaceAtIndex (source - 1) 0 (repeat maxBound)) [1 .. snd b])
  let aux queue@(Heap _ s _ _ _) =
        case s of
             0 -> return ()
             _ -> do
               ((dist, u), queue') <- deleteMin queue
               when(u/=target) $ do
                 let edges = cities ! u
                 forM_ edges $
                   \(v, weight) -> 
                     let dist_thru_u = dist + weight
                     in readArray minDist v >>=
                        \old_dist ->
                          when (dist_thru_u < old_dist) $
                          writeArray minDist v dist_thru_u >>
                          decreaseTo dist_thru_u v queue'
                 aux queue'
  aux heap
  readArray minDist target
  where b = bounds cities

solve :: Array Int [(Int, Int)] -> Int -> Int -> Int
solve cities source target =
  dijkstra source target cities

list2Tuple :: [a] -> (a, a)
list2Tuple (x1:x2:_) = (x1, x2)

process :: Int -> [String] -> IO ()
process s ls0
  | s == 0 = return ()
  | otherwise =
    let (l:ls) = ls0
        (raws, ls') = processCity (read l) ([], ls)
        m = M.fromList (map fst raws `zip` [1..])
        (r:ls'') = ls'
        (paths, ls''') = processPath (read r) m ([], ls'')
        cities = listArray (1, length raws) (map snd raws)
    in forM_ paths (print . uncurry (solve cities)) >> process (s-1) (tail ls''')

processCity :: Int -> ([(String, [(Int, Int)])], [String]) -> ([(String, [(Int, Int)])], [String])
processCity num (cities, ls)
  | num == 0 = (cities, ls)
  | otherwise = let (name:p:ls') = ls
                    (neighbours, ls'') = splitAt (read p) ls'
                    (res, ls''') = processCity (num - 1) (cities, ls'')
                in ((name, map (list2Tuple . map read . words) neighbours):res, ls''')

processPath :: Int -> M.Map String Int -> ([(Int, Int)], [String]) -> ([(Int, Int)], [String])
processPath n m (paths, ls)
  | n == 0 = (paths, ls)
  | otherwise = let (l:ls') = ls
                    [s, t] = map (m M.!) (words l)
                    (res, ls'') = processPath (n - 1) m (paths, ls')
                in ((s, t):res, ls'')

main :: IO ()
main = getContents >>= (\(l:ls) -> process (read l) ls) . lines

