module Grains (square, total) where

import Data.Bits (Bits (shiftL))

square :: Integer -> Maybe Integer
square n = if n < 1 || n > 64 then Nothing else Just $ 1 `shiftL` fromIntegral (n - 1)

total :: Integer
total = 1 `shiftL` 64 - 1
