{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Control.Monad.ST
import Control.Monad (when)
import Data.Array.ST
import Data.Array
import Data.List (intercalate)
import Data.Foldable(for_)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans (liftIO)

readInt :: String -> Int
readInt = read

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

processLines :: [String] -> [(Array Int Char, Array Int Char, Array Int Int)]
processLines (t:ls) = go (readInt t) ls []
  where go :: Int -> [String]
           -> [(Array Int Char, Array Int Char, Array Int Int)]
           -> [(Array Int Char, Array Int Char, Array Int Int)]
        go t ls problems
          | t == 0 = problems
          | otherwise = let (_:keys:letters:remains) = ls
                            (frequencies, remains') = splitAt (length letters) remains
                            kl = length keys
                            ll = length letters
                        in (listArray (0, kl - 1) keys, listArray (0, ll - 1) letters, listArray (0, ll - 1) (map readInt frequencies)) : go (t - 1) remains' problems

ikeyb :: Array Int Char -> Array Int Char -> Array Int Int -> String
ikeyb keys letters frequencies =
  let keysLength = length $ elems keys
      lettersLength = length $ elems letters
      solutions = runST $ do
        bestPrices <- newArray ((0, 0), (keysLength - 1, lettersLength - 1)) maxBound :: ST s (STUArray s (Int, Int) Int)
        solution <- newArray ((0, 0), (keysLength - 1, lettersLength - 1)) (-1) :: ST s (STUArray s (Int, Int) Int)

        for_ ((,) <$> [0 .. keysLength - 1] <*> [0 .. lettersLength - 1]) $ \(i, j) ->
          if i == 0
          then
            if j == 0
            then
              writeArray bestPrices (i, j) (frequencies ! j) >>
              writeArray solution (i, j) 0
            else
              (\t -> t + (frequencies ! j) * (j+1)) <$> readArray bestPrices (i, j - 1) >>=
              writeArray bestPrices (i, j) >>
              writeArray solution (i, j) 0
          else
            readArray bestPrices (i - 1, j) >>=
            \p ->
              when (p < maxBound) $ do
              let currentPrices = 0 : zipWith (+) currentPrices (map (\k -> (frequencies ! k) * (k - j)) [j+1 .. lettersLength - 1])
              for_ (tail currentPrices `zip` [j+1 .. lettersLength - 1])
              $ \(currentPrice, k) ->
                (+currentPrice) <$> readArray bestPrices (i-1, j) >>=
                \possibleSolution -> readArray bestPrices (i, k) >>=
                                     \bp -> when(possibleSolution < bp) $
                                     writeArray bestPrices (i,k) possibleSolution >>
                                     writeArray solution (i,k) (j+1)
        listArray ((0,0), (keysLength-1, lettersLength-1)) <$> getElems solution
      printSolutions :: Int -> Int -> WriterT String Maybe ()
      printSolutions k l
        | k < 0 = tell ""
        | otherwise = do
            printSolutions (k-1) (solutions ! (k,l) - 1)
            tell (keys!k : ": ")
            for_ [solutions ! (k, l) .. l] $ \i ->
              tell [letters!i]
            tell "\n"
      Just result = execWriterT (printSolutions (keysLength - 1) (lettersLength - 1))
  in result

main :: IO ()
main = getContents >>= putStrLn . joinLines . results . lines
  where
    results = withIndex . map (uncurry3 ikeyb) . processLines
    withIndex xs = map (\(i, x) -> "Keypad #" ++ show i ++ ":\n" ++ x ++ "\n") (zip [(1::Int)..] xs)
    joinLines = concat
