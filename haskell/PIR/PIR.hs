import Data.List (intercalate)
import Text.Printf (printf)

pir :: Double -> Double -> Double -> Double -> Double -> Double -> Double
pir a b c d e f = let d' = b*b + c*c - d*d
                      e' = a*a + c*c - e*e
                      f' = a*a + b*b - f*f
                  in (sqrt (4*a*a*b*b*c*c - a*a*d'*d' - b*b*e'*e' - c*c*f'*f' + d'*e'*f'))/12

readDouble :: String -> Double
readDouble = read

main :: IO ()
main = getContents >>= putStrLn . (intercalate "\n") . results . tail . lines
  where pirl (a:b:c:d:e:f:_) = pir a b c d e f
        results = map (format . pirl . (map readDouble) . words)
        format = printf "%0.4f"
