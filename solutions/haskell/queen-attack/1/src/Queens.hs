module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString qW qB = unlines [unwords [board x y | y <- [0 .. 7]] | x <- [0 .. 7]]
  where
    board r c
      | Just (r, c) == qW = "W"
      | Just (r, c) == qB = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack qW qB = fst qW == fst qB || snd qW == snd qB || abs (fst qW - fst qB) == abs (snd qW - snd qB)
