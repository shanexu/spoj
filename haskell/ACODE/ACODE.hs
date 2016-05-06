import Data.List (intercalate, foldl')

acode :: String -> Int
acode [] = 1
acode [_] = 1
acode xs =
  let pairs = init xs `zip` tail xs
      cal (n1, n2) (x1, x2) =
        ((if x2 /= '0' then n1 else 0) + (if (x1 == '2' && x2 > '6') || (x1 > '2') || (x1 == '0') then 0 else n2), n1)
  in fst $ foldl' cal (1, 1) pairs

main :: IO ()
main = getContents >>= putStrLn . join . results . init . lines
  where results = map (show . acode)
        join = intercalate "\n"

