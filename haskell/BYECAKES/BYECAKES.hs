{-# OPTIONS_GHC -optc-O2 #-}

problem :: String -> ([Int], [Int])
problem = splitAt 4 . map read . words

solve :: [Int] -> [Int] -> [Int]
solve s s' = let m = maximum $ zipWith (\x x' -> ceiling (fromIntegral x / fromIntegral x')) s s'
             in zipWith (\x x' -> m * x' - x) s s'

format :: [Int] -> String
format = unwords . map show

main :: IO ()
main = interact $ unlines . map (format . uncurry solve . problem) . init . lines
