{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 -optc-O2 #-}
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Unsafe     as BU
import qualified Data.ByteString.Char8      as S -- 'Strict'
import qualified Data.ByteString.Lazy.Char8 as L -- 'Lazy'
 
main :: IO ()
main = do
  ss <- L.getContents -- done with IO now.
  let (l,ls) = L.break (=='\n') ss
      -- don't need count, we're allocating lazily
      k      = fst . fromJust . L.readInt . last . L.split ' ' $ l
      file   = L.toChunks (L.tail ls) -- a lazy list of strict cache chunks
  print $ process k 0 file
 
-- Optimised parsing of strict bytestrings representing \n separated numbers.
--
-- we have the file as a list of cache chunks align them on \n boundaries, and
-- process each chunk separately when the next chunk is demanded, it will be
-- read in.
process :: Int -> Int -> [S.ByteString] -> Int
process _ i []      = i
process k !i (s:t:ts)
  | S.last s /= '\n' = process k (add k i s') ts'
  where
    (s',r)  = S.breakEnd (=='\n') s
    (r',rs) = S.break    (=='\n') t
    ts'     = S.concat [r,r',S.singleton '\n'] : BU.unsafeTail rs : ts
process k i (s: ss) = process k (add k i s) ss
 
-- process a single cache-sized chunk of numbers, \n aligned
add :: Int -> Int -> S.ByteString -> Int
add k i s = fst $ S.foldl' f (i, 0) s
  where f (!a, !n) '\n' | mod n k == 0  = (a+1, 0)
                        | otherwise     = (a,   0)
        f (!a, !n) w  = (a, 10*n+ord w-ord '0')
