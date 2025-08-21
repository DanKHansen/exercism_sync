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
    aliq = sum $ [f | f <- [1 .. div n 2], mod n f == 0]