module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise = Just $ case compare aliq n of
      LT -> Deficient
      EQ -> Perfect
      GT -> Abundant
  where
    aliq = sum $ filter p [1 .. div n 2]
    p i = mod n i == 0