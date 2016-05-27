problem :: [String] -> (Int, Int)
problem [n, x] = (read n, read x)

solve :: Int -> Int -> Maybe ([Int], [Int])
solve n x = go n x 1 ([], [])
  where go :: Int -> Int -> Int -> ([Int], [Int]) -> Maybe ([Int], [Int])
        go n k i (l, r)
          | k == 0 = Just (l, r)
          | i > n = Nothing
          | otherwise = let (nk, re) = k `quotRem` 3
                        in case re of
                             1 -> go n nk (i+1) (l, i:r)
                             2 -> go n (nk+1) (i+1) (i:l, r)
                             _ -> go n nk (i+1) (l, r)

format :: Maybe ([Int], [Int]) -> String
format Nothing = "-1"
format (Just (l , r)) =  f l ++ "\n" ++ f r
  where f = unwords . map show. reverse

main :: IO ()
main = interact(format . uncurry solve . problem . words . head . lines)
