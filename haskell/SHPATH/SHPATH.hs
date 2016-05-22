{-# LANGUAGE FlexibleContexts #-}

import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Control.Monad (foldM, forM_, when)
import qualified Data.Map as M
import qualified Data.Set as S

dijkstra ::  Int -> Int -> Array Int [(Int,Int)] -> Int
dijkstra src target adj_list = runST $ do
  min_distance <- Data.Array.ST.newArray b maxBound :: ST s (STUArray s Int Int)
  writeArray min_distance src 0
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            when(u /= target) $
              let edges = adj_list ! u
                  f vertex_queue (v, weight) = do
                    let dist_thru_u = dist + weight
                    old_dist <- readArray min_distance v
                    if dist_thru_u >= old_dist then
                      return vertex_queue
                      else do
                      let vertex_queue' = S.delete (old_dist, v) vertex_queue
                      writeArray min_distance v dist_thru_u
                      return $ S.insert (dist_thru_u, v) vertex_queue'
              in
                foldM f vertex_queue' edges >>= aux
  aux (S.singleton (0, src))
  readArray min_distance target
  where b = bounds adj_list

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

