module Darts (score) where

import Data.List (find)

score :: Float -> Float -> Int
score x y = maybe 0 snd $ find (\(r, _) -> d <= r) ranges
  where
    d = sqrt (x * x + y * y)
    ranges = [(1, 10), (5, 5), (10, 1)]