import Data.List(foldl')

solve :: String -> Int
solve = fst . foldl' f (0, (0, 0)) . words
  where f (maxCount, (currentCount, prevLength)) currentWord =
          let currentWordLength = length currentWord
              newCurrentCount = if currentWordLength == prevLength then currentCount + 1 else 1
              newMaxCount = max newCurrentCount maxCount
          in (newMaxCount, (newCurrentCount, currentWordLength))

format :: Int -> String
format = show

main :: IO ()
main = interact $ unlines . map (format . solve) . tail . lines
