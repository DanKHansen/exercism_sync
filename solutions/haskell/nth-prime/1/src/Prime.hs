module Prime (nth) where

primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]

nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = Just . fromIntegral $ last (take n primes)

isPrime :: Int -> Bool
isPrime x = all (\p -> mod x p /= 0) (takeWhile (\p -> p ^ (2 :: Int) <= x) primes)
