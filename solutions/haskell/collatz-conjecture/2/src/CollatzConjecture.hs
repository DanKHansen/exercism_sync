module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz n
  | n <= 0 = Nothing
  | even n = fmap (+ 1) (collatz (div n 2))
  | otherwise = fmap (+ 1) (collatz (3 * n + 1))
