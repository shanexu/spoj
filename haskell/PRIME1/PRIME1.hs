{-# OPTIONS_GHC -optc-O2 #-}

s235 :: [Int]
s235 = 7 : 11 : 13 : 17 : 19 : 23 : 29 : 31 : map (+30) s235 

pseudoPrimes :: [Int]
pseudoPrimes = 2 : 3 : 5 : s235

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | otherwise = go n pseudoPrimes (floor . sqrt . fromIntegral $ n)
  where
    go :: Int -> [Int] -> Int -> Bool
    go x (p:ps) l
          | p > l          = True
          | x `mod` p == 0 = False
          | otherwise      = go x ps l

gen235Sequence :: Int -> Int -> [Int]
gen235Sequence l u = let ss = if l < 7
                               then
                                 2 : 3 : 5 : s235
                               else
                                 let n = l `div` 30
                                 in if n * 30 + 7 <= l then map (30*n+) s235 else map (30*(n-1)+) s235
                     in takeWhile (<=u) . dropWhile (<l) $ ss

genPrimeSequence :: Int -> Int -> [Int]
genPrimeSequence l = filter isPrime . gen235Sequence l

readInt :: String -> Int
readInt = read

main :: IO ()
main = getContents >>= putStr . unlines . map (unlines . map show . (\[l, u] -> genPrimeSequence l u) . (map readInt . words)) . tail . lines


