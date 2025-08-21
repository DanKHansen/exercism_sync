module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = map (\r -> map (`pascal` r) [0 .. r]) [0 .. fromIntegral (x - 1)]

pascal :: Integer -> Integer -> Integer
pascal c r =
  if (r == 0) || (r == c) || (c == 0)
    then 1
    else pascal (c - 1) (r - 1) + pascal c (r - 1)
