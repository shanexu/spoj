import Data.List(foldl')

problems :: [String] -> [[Integer]]
problems = snd . foldl' f (False, [])
  where f (isEven, ps) l = (not isEven, if isEven then (map read . words $ l) : ps else ps)

tree1 :: [[Integer]] -> [Integer]
tree1 = foldl' (\res p -> solve p : res) []

solve :: [Integer] -> Integer
solve ts = (product [1..len] `div`) . fst . travel (0, len + 1) $ (1, ts)
  where len = fromIntegral . length $ ts

--        (lower,     upper) -> (number , remains)   -> (number', remains')
travel :: (Integer, Integer) -> (Integer, [Integer]) -> (Integer, [Integer])
travel _ p@(_, []) = p
travel (l, u) p@(n, t:rs) = if t > l && t < u
                            then travel (t, u) $ travel (l, t) ((u - l - 1)*n, rs)
                            else p

main :: IO ()
main = interact $ unlines . map show . tree1 . problems . tail . lines
