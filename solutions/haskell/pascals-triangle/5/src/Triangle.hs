module Triangle (rows) where

rows n = take n $ iterate (\xs -> zipWith (+) (0 : xs) (xs ++ [0])) [1]