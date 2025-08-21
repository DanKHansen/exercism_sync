module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n $ iterate (\xs -> zipWith (+) (0 : xs) (xs ++ [0])) [1]