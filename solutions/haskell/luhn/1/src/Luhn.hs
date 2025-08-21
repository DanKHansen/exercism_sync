module Luhn (isValid) where

import Data.Char (digitToInt, isSpace)

isValid :: String -> Bool
isValid n = (sum norm `mod` 10 == 0) && (length noSpace > 1)
  where
    noSpace = filter (not . isSpace) n
    norm =
      [ let v = digitToInt d * (1 + (i `mod` 2))
         in (v + if v > 9 then -9 else 0) `mod` 10
        | (d, i) <- zip (reverse noSpace) [0 ..]
      ]