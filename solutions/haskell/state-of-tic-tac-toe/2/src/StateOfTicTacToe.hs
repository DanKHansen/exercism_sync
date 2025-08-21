module StateOfTicTacToe (gameState, GameState (..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | cs 'O' > cs 'X' || cs 'X' > (cs 'O' + 1) || (win 'X' && win 'O') = Impossible
  | win 'X' = WinX
  | win 'O' = WinO
  | cs 'O' + cs 'X' == 9 = Draw
  | otherwise = Ongoing
  where
    cs c = sum $ map (length . filter (== c)) board
    win c =
      [c, c, c] `elem` board
        || [c, c, c] `elem` transpose board
        || [c, c, c] == [head (head board), board !! 1 !! 1, last (last board)]
        || [c, c, c] == [last (head board), board !! 1 !! 1, head (last board)]
