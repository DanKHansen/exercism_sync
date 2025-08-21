module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n pascals

pascals :: [[Integer]]
pascals = [1] : map (\r -> zipWith (+) (0 : r) (r ++ [0])) pascals