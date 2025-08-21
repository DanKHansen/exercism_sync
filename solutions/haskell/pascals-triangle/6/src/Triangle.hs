module Triangle (rows) where

rows :: Int -> [[Integer]]
rows = flip take . iterate (\xs -> zipWith (+) (0 : xs) (xs ++ [0])) $ [1]