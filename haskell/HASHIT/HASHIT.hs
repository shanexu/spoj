import Data.Char (ord)
import Data.Array.IO
import Data.Array.ST
import Control.Monad.ST
import Data.Foldable(forM_)
import Control.Applicative
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.List (intercalate)

readInt :: String -> Int
readInt = read

processLines :: [String] -> [[String]]
processLines (t:ls) = go (readInt t) ls []
  where go t ls problems
          | t == 0 = problems
          | otherwise = let (n1:remains) = ls
                            (ops, remains') = splitAt (readInt n1) remains
                        in ops : go (t - 1) remains' problems

hash :: String -> Int
hash str = h str `mod` 101
  where h xs = (19*) . sum $ zipWith (\n b -> n * ord b) [1..(length xs)] xs

hashN :: Int -> Int -> Int
hashN h n = (h + n*n + 23*n) `mod` 101

hashIt :: [String] -> String
hashIt ops = runST $ do
  table <- newArray (0, 100) Nothing :: ST s (STArray s Int (Maybe String))
  forM_ ops $ \op ->
    case op of
         'A':'D':'D':':':str -> add table str (hash str) 0 Nothing
         'D':'E':'L':':':str -> del table str (hash str) 0
  format table

add :: STArray s Int (Maybe String) -> String -> Int ->  Int -> Maybe Int -> ST s ()
add table str h t p
  | t > 19 = return ()
  | otherwise = do
      let h' = hashN h t
      readArray table h' >>= \v ->
        case v of
             Nothing ->
               if isJust p then add table str h (t+1) p else writeArray table h' (Just str) >> add table str h (t+1) (Just h')
             Just s ->
               if s == str
               then
                 case p of
                   Just pos -> writeArray table pos Nothing
                   _ -> return ()
               else
                 add table str h (t+1) p

del :: STArray s Int (Maybe String) -> String -> Int ->  Int -> ST s ()
del table str h t
  | t > 19 = return ()
  | otherwise = do
      let h' = hashN h t
      readArray table h' >>= \v ->
        case v of
             Nothing -> del table str h (t+1)
             Just s -> if s == str
                       then writeArray table h' Nothing
                       else del table str h (t+1)

format :: STArray s Int (Maybe String) -> ST s String
format table = joinLines . (\res -> show (length res) : res) . map (\ (i, Just s) -> show i ++ ":" ++ s) . filter (isJust . snd) . zip [0..100] <$> getElems table

joinLines :: [String] -> String
joinLines = intercalate "\n"

main :: IO ()
main = getContents >>= putStrLn . joinLines . results . processLines . lines
  where results = map hashIt
