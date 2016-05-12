{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Data.Maybe (isJust)
import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Data.Foldable(forM_)
import Control.Monad (when, forM)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy as BLW

type Weight = Int
type Value = Int

type Coin = (Value, Weight)

inf :: Int
inf = 33554432

breakPigBank :: STUArray s Weight Value -> Weight -> [Coin] -> ST s Value
breakPigBank ts w cs = do
  forM_ [1..w] $ \i -> writeArray ts i inf
  forM_ cs $
    \(v , g) ->
      forM_ [g..w] $
      \i -> do
        let i' = i - g
        readArray ts i' >>= \t' ->
                              readArray ts i >>= \t ->
                                                   writeArray ts i ((t' + v) `min` t)
  readArray ts w

solve :: [(Weight, [Coin])] -> [Value]
solve ps = runST $ do
  ts <- newArray (0, 10050) inf :: ST s (STUArray s Weight Value)
  writeArray ts 0 0
  forM ps $ \(w, cs) -> breakPigBank ts w cs

takeLine :: BLW.ByteString -> (BLW.ByteString, BLW.ByteString)
takeLine ls = let (l, ls') = BLW.break (==10) ls
              in (l, BLW.tail ls')

takeLines :: Int -> BLW.ByteString -> ([BLW.ByteString], BLW.ByteString)
takeLines t ls = go t ([], ls)
  where
    go n pair
      | n <= 0 = pair
      | otherwise = let (results, ss) = pair
                        (l, ss') = takeLine ss
                        (results', ss'') = go (n-1) (results, ss')
                    in (l : results', ss'')

readInt :: BLW.ByteString -> Int
readInt = BLW.foldl' (\x c -> 10 * x + fromIntegral c - 48) 0

problems :: Int -> BLW.ByteString -> [(Weight, [Coin])]
problems t ls = go t ls []
  where go t ls ps
          | t <= 0 = ps
          | otherwise = let (!l1, !ls1) = takeLine ls
                            [!w1, !w2] = map readInt . BLW.split 32 $ l1
                            weight = w2 - w1
                            (!l2, !ls2) = takeLine ls1
                            c = readInt l2
                            (!cs, !ls3) = takeLines c ls2
                            coins = map ((\[!x1,!x2] -> (x1,x2)) . map readInt . BLW.split 32) cs
                        in (weight, coins) : go (t - 1) ls3 ps

joinLines :: [String] -> String
joinLines = intercalate "\n"

main :: IO ()
main = do
  (l, ls) <- BLW.break (==10) `fmap` BLW.getContents
  let t = readInt l
      ps = problems t (BLW.tail ls)
      results = solve ps
      format m | m == inf = "This is impossible."
               | otherwise = "The minimum amount of money in the piggy-bank is " ++ show m ++ "."

  putStrLn (joinLines . map format $ results)

