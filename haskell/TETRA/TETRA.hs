{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -optc-O2 #-}

import Text.Printf(printf)

volume :: Double -> Double -> Double -> Double -> Double -> Double -> Double
volume !u !v !w !x !y !z =
  let u1 = v * v + w * w - x * x
      v1 = w * w + u * u - y * y
      w1 = u * u + v * v - z * z
  in sqrt (4.0*u*u*v*v*w*w - u*u*u1*u1 - v*v*v1*v1 - w*w*w1*w1 + u1*v1*w1) / 12.0

surface :: Double -> Double -> Double -> Double
surface !a !b !c =
  sqrt ((a + b + c) * (-a + b + c) * (a - b + c) * (a + b - c)) / 4.0;

radious :: Double -> Double -> Double -> Double -> Double -> Double -> Double
radious !a !b !c !x !y !z =
  let sur = surface a b x + surface a y c + surface b z c + surface x y z
  in (volume a b c z y x) * 3.0 / sur

radiousL :: [Double] -> Double
radiousL ![a,b,c,x,y,z] = radious a b c x y z

format :: Double -> String
format v
  | isNaN v = "0.0000"
  | otherwise = printf "%.4f" v

main :: IO ()
main = getContents >>= putStr . unlines . map (format . radiousL . map read .  words) . tail . lines 
