{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -optc-O2 #-}
import Data.List(foldl')
import Data.Char
import qualified Data.ByteString.Unsafe     as BU
import qualified Data.ByteString.Char8      as S -- 'Strict'
import qualified Data.ByteString.Lazy.Char8 as L -- 'Lazy'

zero :: Int -> Int
zero 0 = 0
zero !n = go n 0
  where go 0 !count = count
        go !num !count =
          let q = num `quot` 5
          in go q (q + count)

main :: IO ()
main = do
  ss <- L.getContents -- done with IO now.
  let (_,ls) = L.break (=='\n') ss
      -- don't need count, we're allocating lazily
      file   = L.toChunks (L.tail ls) -- a lazy list of strict cache chunks
      res = process [] file
      strs = foldl' (\s n -> show n : s) [] res
    in putStr . unlines $ strs

-- Optimised parsing of strict bytestrings representing \n separated numbers.
--
-- we have the file as a list of cache chunks align them on \n boundaries, and
-- process each chunk separately when the next chunk is demanded, it will be
-- read in.
process :: [Int] -> [S.ByteString] -> [Int]
process i []      = i
process !i (s:t:ts)
  | S.last s /= '\n' = process (add i s') ts'
  where
    (s',r)  = S.breakEnd (=='\n') s
    (r',rs) = S.break    (=='\n') t
    ts'     = S.concat [r,r',S.singleton '\n'] : BU.unsafeTail rs : ts
process i (s: ss) = process (add i s) ss

-- process a single cache-sized chunk of numbers, \n aligned
add :: [Int] -> S.ByteString -> [Int]
add i s = fst $ S.foldl' f (i, 0) s
  where f (!a, !n) '\n' = (zero n :a, 0)
        f (!a, !n) w  = (a, 10*n+ord w-ord '0')
