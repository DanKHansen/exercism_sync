module StateOfTicTacToe (gameState, GameState (..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState b
  | invalid = Impossible
  | win 'X' = WinX
  | win 'O' = WinO
  | turns 'O' + turns 'X' == 9 = Draw
  | otherwise = Ongoing
  where
    turns c = sum $ map (length . filter (== c)) b
    win c = [c, c, c] `elem` (b ++ transpose b ++ diags)
    diags = [[b !! i !! i | i <- [0 .. 2]], [b !! i !! (2 - i) | i <- [0 .. 2]]]
    invalid = turns 'O' > turns 'X' || turns 'X' > turns 'O' + 1 || (win 'X' && win 'O')