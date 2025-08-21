module StateOfTicTacToe (gameState, GameState (..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | invalidState = Impossible
  | win 'X' = WinX
  | win 'O' = WinO
  | turns 'O' + turns 'X' == 9 = Draw
  | otherwise = Ongoing
  where
    turns c = sum $ map (length . filter (== c)) board
    win c = [c, c, c] `elem` (board ++ transpose board ++ diagonals)
    diagonals = [[board !! i !! i | i <- [0 .. 2]], [board !! i !! (2 - i) | i <- [0 .. 2]]]
    invalidState = turns 'O' > turns 'X' || turns 'X' > turns 'O' + 1 || (win 'X' && win 'O')