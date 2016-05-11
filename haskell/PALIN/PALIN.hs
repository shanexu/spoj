import Control.Applicative
import Data.List (intercalate)
import Data.Char (digitToInt, intToDigit)

type Digit = Char

readInt :: String -> Int
readInt = read

problems :: [String] -> [[Digit]]
problems (t:remains) = fst (splitAt (readInt t) remains)

solve :: [String] -> [String]
solve = map palin

joinLines :: [String] -> String
joinLines = intercalate "\n"

digitsToInts :: [Digit] -> [Int]
digitsToInts = map digitToInt

intsToDigits :: [Int] -> [Digit]
intsToDigits = map intToDigit

increaseOne :: [Int] -> [Int]
increaseOne xs = reverse $ go (reverse xs)
  where
    split t = (t `div` 10, t `mod` 10)
    go [] = [1]
    go (x:xs) = let (x1, x0) = split (x + 1)
                in if x1 == 0
                   then x0 : xs
                   else x0 : go xs

palin :: String -> String
palin = intsToDigits . nextPalindrome . digitsToInts

nextPalindrome :: [Int] -> [Int]
nextPalindrome xs
  | all (==9) xs = 1:replicate (length xs - 1) 0 ++ [1]
  | otherwise =  let l = length xs
                     isOddLength = odd l
                     halfLength = (l + 1) `div` 2
                     (leftHalf, r) = splitAt halfLength xs
                     rightHalf = if isOddLength then last leftHalf:r else r
                     reversedLeftHalf = reverse leftHalf
                     newRightHalf = if reversedLeftHalf > rightHalf
                                    then reversedLeftHalf
                                    else reverse (increaseOne leftHalf)
                 in if isOddLength
                    then init (reverse newRightHalf) ++ newRightHalf
                    else reverse newRightHalf ++ newRightHalf

main :: IO ()
main = getContents >>= putStrLn . joinLines . solve . problems . lines
