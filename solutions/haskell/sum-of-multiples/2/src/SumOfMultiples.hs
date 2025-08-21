module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub $ concatMap multiples factors
  where
    multiples f = if f == 0 then [] else takeWhile (< limit) [f, f * 2 ..]
