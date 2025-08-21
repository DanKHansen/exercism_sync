module Prime (nth) where

primes :: [Int]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Int -> Bool
isPrime x = all (\p -> mod x p /= 0) (takeWhile (\p -> p * p <= x) primes)

nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = Just . fromIntegral $ primes !! (n - 1)
