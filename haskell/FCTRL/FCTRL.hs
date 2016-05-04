import Data.List (intercalate)

zero :: Int -> Int
zero 0 = 0
zero n = zero' n 0
  where zero' 0 count = count
        zero' num count =
          let q = num `div` 5
          in zero' q (q + count)

readInt :: String -> Int
readInt = read

main :: IO ()
main = putStrLn . (intercalate "\n") . (map (show . zero . readInt)) . tail . lines =<< getContents
