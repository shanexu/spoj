import Text.Printf(printf)

problem :: String -> (Double, Double, Double, Double)
problem = (\[a, b, c, d] -> (a, b, c, d)) . map read . words

solve :: (Double, Double, Double, Double) -> (Double, Double)
solve (a, ga, gb, gc) = let b = a * ga / gb
                            c = a * ga / gc
                            l = (a + b + c) / 2
                            s = sqrt (abs (l*(l-a)*(l-b)*(l-c)))
                            r = a * b * c / 4 / s
                            hg = 2 * sqrt(abs (9*r*r-a*a-b*b-c*c)) / 3
                        in (s, hg)

format :: (Double, Double) -> String
format (s, hg) = printf "%.3f %.3f " s hg

main :: IO ()
main = interact $ unlines . map (format . solve . problem) . tail . lines
