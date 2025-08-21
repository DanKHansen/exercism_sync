module Triangle (rows) where

rows = flip take . iterate (\xs -> zipWith (+) (0 : xs) (xs ++ [0])) $ [1]