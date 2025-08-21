module Grains (square, total) where

import Data.Bits (Bits (shiftL))
import Data.Maybe (mapMaybe)

square :: Integer -> Maybe Integer
square n
  | n >= 1 && n < 65 = Just (shiftL 1 (fromIntegral (n - 1)))
  | otherwise = Nothing

total :: Integer
total = sum (mapMaybe square [1 .. 64])
