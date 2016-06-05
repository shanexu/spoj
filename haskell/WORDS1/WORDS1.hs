{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad(foldM, forM_, liftM2)
import Data.List(foldl')

data V a = V { vertexIdx :: a
             , inDegree :: Int
             , inEdges :: [a]
             , outDegree :: Int
             , outEdges :: [a]} deriving (Eq, Show)

emptyV :: a -> V a
emptyV idx = V idx 0 [] 0 []

isEmptyV :: V a -> Bool
isEmptyV v = inDegree v == 0 && outDegree v == 0

(~>) :: Eq a =>  V a -> V a -> (V a, V a)
v1 ~> v2 = if vertexIdx v1 == vertexIdx v2
           then let v = v1 { inDegree = inDegree v1 + 1
                           , outDegree = outDegree v1 + 1
                           , inEdges = vertexIdx v1 : inEdges v1
                           , outEdges = vertexIdx v1 : outEdges v1 }
                in (v, v)
           else (v1 { outDegree = outDegree v1 + 1
                    , outEdges = vertexIdx v2 : outEdges v1}
                ,v2 { inDegree = inDegree v2 + 1
                    , inEdges = vertexIdx v1 : inEdges v2})

findVertex :: Array Char (V Char) -> (Char, Int)
findVertex arr = foldl' (\(ch, n) c ->
                           let v = arr ! c
                           in
                             if isEmptyV v
                             then (ch, n)
                             else
                               if n == 0
                               then (c, 1)
                               else (ch, n+1)) (' ', 0) ['a'..'z']

choose :: a -> a -> Bool -> a
choose v1 v2 b = if b then v1 else v2

isConnected :: Array Char (V Char) -> ST s Bool
isConnected arr = do
  let (s, n) = findVertex arr
      go [] step _ = return (step == n)
      go (ch:chs) step tracks =
        if step == n
        then return True
        else
          let v = arr ! ch
          in foldM (\(us, st) u -> readArray tracks u >>= \vd ->
                                                            if vd
                                                            then return (us, st)
                                                            else writeArray tracks u True >> return (u:us, st+1)) (chs, step) (inEdges v ++ outEdges v) >>=
             \(chs', step') -> go chs' step' tracks
  (newArray ('a', 'z') False :: ST s (STUArray s Char Bool)) >>= go [s] 0

problem :: [String] -> ST s (Array Char (V Char))
problem strs = do
  arr <- newListArray ('a', 'z') (map emptyV ['a'..'z']) :: ST s (STArray s Char (V Char))
  forM_ strs $ \s -> do
    let h = head s
        l = last s
    if h == l
      then (\v -> v ~> v) `fmap` readArray arr h >>= \(v, _) -> writeArray arr h v
      else liftM2 (~>) (readArray arr h) (readArray arr l) >>= \(v1, v2) -> writeArray arr h v1 >> writeArray arr l v2
  freeze arr

solve :: [String] -> String
solve strs = runST $ do
  arr <- problem strs
  fmap
    (choose "Ordering is possible." "The door cannot be opened." .
       choose (checkDegree arr) False)
    (isConnected arr)

checkDegree :: Array Char (V Char) -> Bool
checkDegree arr = let go i (o_i, i_o)
                        | o_i > 1 || i_o > 1 = False
                        | i > 'z' = True
                        | otherwise =
                            let vi = arr ! i
                            in case outDegree vi - inDegree vi of
                                 0  -> go (succ i) (o_i, i_o)
                                 1  -> go (succ i) (o_i + 1, i_o)
                                 -1 -> go (succ i) (o_i, i_o + 1)
                                 _  -> False
                  in go 'a' (0 :: Int, 0 :: Int)

process :: [String] -> IO ()
process (l:ls) = go (read l) ls
  where
    go 0 _ = return ()
    go t (str:strs) = let n = read str
                          (ws, ls') = splitAt n strs
                      in (putStrLn . solve $ ws) >> go (t-1) ls'

main :: IO ()
main = getContents >>= process . lines
