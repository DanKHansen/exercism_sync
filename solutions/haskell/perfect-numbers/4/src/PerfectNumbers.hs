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
    aliq = sum $ [k | k <- [1 .. (n - 1)], n `mod` k == 0]

-- p i = mod n i == 0