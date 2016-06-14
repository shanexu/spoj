{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -optc-O2 #-}

solve :: Integer -> Integer
solve n = let (!s, !r) = n `quotRem` 9
          in s * 81 + r*r

main :: IO ()
main = interact $ unlines . map (show . solve . read) . lines
