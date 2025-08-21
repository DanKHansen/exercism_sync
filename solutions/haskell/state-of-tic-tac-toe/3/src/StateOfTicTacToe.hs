module StateOfTicTacToe (gameState, GameState (..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | invalidState = Impossible
  | win 'X' = WinX
  | win 'O' = WinO
  | allFilled = Draw
  | otherwise = Ongoing
  where
    counts c = sum $ map (length . filter (== c)) board
    win c = [c, c, c] `elem` (board ++ transpose board ++ diagonals)
    invalidState = counts 'O' > counts 'X' || counts 'X' > counts 'O' + 1 || (win 'X' && win 'O')
    allFilled = counts 'O' + counts 'X' == 9
    diagonals =
      [ [head (head board), board !! 1 !! 1, last (last board)],
        [last (head board), board !! 1 !! 1, head (last board)]
      ]