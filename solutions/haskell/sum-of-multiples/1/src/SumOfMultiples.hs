module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ nub $ concatMap (takeWhile (< limit) . (\f -> if f == 0 then [0] else [f, f * 2 ..])) factors
