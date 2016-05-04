import Data.Char (ord)
import Data.Array.IO

data HashOp = ADD String | DEL String

hash :: String -> Int
hash str = (h str) `mod` 101
  where h xs = (19*) . sum $ zipWith (\n b -> n * (ord b)) [1..(length xs)] xs

hashIt :: [HashOp] -> IOArray Int (Maybe String)
-- hashIt ops = do
--   arr <- newArray (0, 100) Nothing :: IOArray Int (Maybe String)
--   arr

main = mapM_ (putStrLn) . tail . lines =<< getContents
