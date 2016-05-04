import Control.Monad (replicateM_)
import Data.Map.Strict
import Data.Char(isAsciiLower)

onp :: String -> String
onp [] = ""
onp str@(_:_) = let (result, _, _) = onp' "" "" str
                in result
  where onp' result ops [] = (result ++ ops, "", "")
        onp' result ops (x:xs)
          | isAsciiLower x = onp' (result ++ [x]) ops xs
          | x == lp = let (result', _, remains) = onp' "" "" xs
                      in onp' (result ++ result' ++ ops) "" remains
          | x == rp = ((result ++ ops), "", xs)
          | otherwise = onp' result (insertOp x ops) xs
        insertOp op [] = [op]
        insertOp op ops@(x:xs)
          | opMap ! op >= opMap ! x = op:ops
          | otherwise = x:(insertOp op xs)
        lp = '('
        rp = ')'
        opMap = fromList $ zip "+-*/^" [0..4]

main :: IO ()
main = (read `fmap` getLine :: IO Int) >>=
  \t -> replicateM_ t $ onp `fmap` getLine >>= putStrLn

