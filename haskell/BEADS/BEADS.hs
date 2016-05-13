import Data.Array

beads :: String -> Int
beads str = let l = length str
                arr = listArray (0, l - 1) str
                (p, q, _) = go (0, 0, 0)
                go t@(i, j, k)
                  | k == l || i == l || j == l = t
                  | i == j = go (i, j + 1, k)
                  | otherwise = case arr ! ((i + k) `mod` l) `compare` (arr ! ((j + k) `mod` l)) of
                                  LT -> go (i, j + k + 1, 0)
                                  EQ -> go (i, j, k + 1)
                                  GT -> go (i + k + 1, j, 0)
            in min p q + 1

main :: IO ()
main = getContents >>= putStr . unlines . map (show . beads) . tail . lines
