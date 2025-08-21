module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = loop n 0

loop :: Integer -> Integer -> Maybe Integer
loop n acc
  | n <= 0 = Nothing
  | n == 1 = Just acc
  | even n = loop (div n 2) (acc + 1)
  | otherwise = loop (3 * n + 1) (acc + 1)
