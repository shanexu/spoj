import Data.List (intercalate, foldl')
import Control.Monad
import Prelude hiding (Rational)

data Rational = Int :/ Int deriving (Show)

instance Eq Rational where
  (x:/y) == (x':/y') = x*y' == y*x'

data Complex = Rational :+ Rational deriving (Show)

instance Eq Complex where
  (x:+y) == (x':+y') = x == x' && y == y'

(%) :: Int -> Int -> Rational
x % y = if q * y == x then q :/ 1 else x :/y
  where q = x `div` y

instance Fractional Rational where
  (x:/y) / (x':/y')   =  (x*y') % (y*x')

instance Num Rational where
  (x:/y) + (x':/y')   =  (x*y' + y*x') % (y*y')
  (x:/y) - (x':/y')   =  (x*y' - y*x') % (y*y')
  (x:/y) * (x':/y')   =  (x*x') % (y*y')
  fromInteger n       =  fromInteger n :/ 1

instance Fractional Complex  where
  (x:+y) / (x':+y')   =  (x''/r) :+ (y''/r)
    where r = x'*x' + y'*y'
          x'' = x*x' + y*y'
          y'' = x'*y - x*y'
  fromRational n      =  fromRational n :+ 0

instance Num Complex  where
  (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
  (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
  (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
  fromInteger n       =  fromInteger n :+ 0

maxA :: Complex -> Int
maxA ((x:/y):+(x':/y')) = floor . sqrt $ ((fromIntegral (x*x))/(fromIntegral (y*y)) + (fromIntegral (x'*x'))/(fromIntegral (y'*y')) - 1)

solve :: Complex -> Complex -> Maybe [Int]
solve x b = go x b (maxA b) [] 0

go :: Complex -> Complex -> Int -> [Int] -> Int -> Maybe [Int]
go (0:+0) _ _ res _ = Just res
go x b ma res t
  | t == 100 = Nothing
  | otherwise = foldl' (\r i ->
                          let x'@((_:/m) :+ (_:/n)) = (x - (fromIntegral i)) / b
                          in r `mplus` (if m == 1 && n == 1 then go x' b ma (i:res) (t+1) else Nothing)) Nothing  [0 .. ma]

format :: Maybe [Int] -> String
format Nothing = "The code cannot be decrypted."
format (Just []) = "0"
format (Just as@(_:_)) = intercalate "," (map show as)

main :: IO ()
main = getContents >>= putStr . unlines . map (format . uncurry solve . (\[a,b,c,d] -> ((fromIntegral a) :+ (fromIntegral b), (fromIntegral c) :+ (fromIntegral d))) . map read . words) . tail . lines

