{-# OPTIONS_GHC -optc-O2 #-}
import qualified Data.ByteString.Lazy.Char8 as L -- 'Lazy'
import Data.Maybe

data Tree p v = Node p v (Tree p v) (Tree p v) | Empty deriving (Show, Eq)

insert :: (Ord p, Num v) =>  p -> v -> Tree p v -> Tree p v
insert p v Empty = Node p v Empty Empty
insert p v (Node rp rv left right) = case p `compare` rp of
                                       LT -> Node rp rv (insert p v left) right
                                       GT -> Node rp rv left (insert p v right)
                                       _  -> Node rp (rv+v) left right

printTree :: Tree L.ByteString Int -> IO ()
printTree Empty = return ()
printTree (Node rp rv left right) = printTree left >> L.putStr rp >> print rv >> printTree right

problem :: Int -> L.ByteString -> IO ()
problem 0 _ = return ()
problem n ss = let (l, ss') = L.break (=='\n')  (L.tail ss)
                   k = fst . fromJust . L.readInt $ l
                   (t, ss'') = process k ss' Empty
               in printTree t >> putStr "\n" >> problem (n-1) (L.tail ss'')

process :: Int -> L.ByteString -> Tree L.ByteString Int -> (Tree L.ByteString Int, L.ByteString)
process 0 s t = (t, s)
process n strs t = let (l, strs') = L.break (=='\n') (L.tail strs)
                   in process (n-1) strs' (insert l 1 t)
main :: IO ()
main = do
  ss <- L.getContents
  let (l, ss') = L.break (=='\n') ss
      n = fst . fromJust . L.readInt $ l
  problem n ss'
