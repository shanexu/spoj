import Data.List (intercalate)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l) : (chunksOf n (drop n l))

readInt :: String -> Int
readInt = read

pour1 :: Int -> Int -> Int -> Int
pour1 ca cb c
  | c > ca && c > cb = -1
  | c `mod` (gcd ca cb) /= 0 = -1
  | otherwise = min (pour' ca cb 0 0 0) (pour' cb ca 0 0 0)
  where pour' a b p q count
          | p == c || q == c = count
          | p == 0 = pour' a b a q (count + 1)
          | q < b =  let b_q = b - q
                         pour = if p < b_q then p else b_q
                     in pour' a b (p - pour) (q + pour) (count + 1)
          | q == b = pour' a b p 0 (count + 1)

main :: IO ()
main = getContents >>= putStrLn . (intercalate "\n") . (map $ show . pourl) . (chunksOf 3) . (map readInt) . tail . lines
  where pourl (a:b:c:_) = pour1 a b c
  
