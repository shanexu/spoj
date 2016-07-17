{-# OPTIONS_GHC -optc-O2 #-}

import Data.List(foldl', intercalate)
import Control.Monad(foldM_)

books1 :: [Int] -> Int -> [[Int]]
books1 books scriberCount = devide (search (maximum books) (sum books))
  where search lo hi
          | lo >= hi = lo
          | otherwise = let mid = lo + ((hi - lo) `div` 2)
                            f (total, count) book = let total' = total + book
                                                    in if total' > mid
                                                       then (book, count+1)
                                                       else (total', count)
                            (_, sc) = foldl' f (head books, 1) (tail books)
                        in if sc <= scriberCount
                           then search lo mid
                           else search (mid+1) hi
        devide mid = let fb = head books
                         bl = length books
                         f ((total, count), ss@(s:ss')) (book, i) = let total' = total + book
                                                                    in if total' > mid || (bl - i) < (scriberCount - count)
                                                                       then ((book, count+1), [book]:ss)
                                                                       else ((total', count), (book:s):ss')
                     in snd (foldl' f ((fb, 1), [[fb]]) (tail (zip books [1..] :: [(Int, Int)])))

format :: [[Int]] -> String
format = intercalate " / " . map (unwords . map show)

solveProblems :: [String] -> IO ()
solveProblems = foldM_ f 0
  where f 0 l = let [_, n] = map read . words $ l
                in return n
        f n l = putStrLn (format $ books1 (reverse . map read . words $ l) n) >> return 0

main :: IO ()
main = getContents >>= solveProblems . tail . lines
