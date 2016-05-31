{-# OPTIONS_GHC -optc-O2 #-}

problem :: String -> [Integer]
problem = map read . words

main :: IO ()
main = interact $ unlines . map (show . product . problem) . tail . lines
