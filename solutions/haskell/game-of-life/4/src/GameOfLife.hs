module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick [] = []
tick board = [[applyRules c (liveAdjecents i j) | (j, c) <- zip [0 ..] row] | (i, row) <- zip [0 ..] board]
  where
    rows = length board
    cols = length (head board)

    liveAdjecents i j =
      sum
        [ board !! x !! y
          | x <- [max 0 (i - 1) .. min (rows - 1) (i + 1)],
            y <- [max 0 (j - 1) .. min (cols - 1) (j + 1)],
            (x, y) /= (i, j)
        ]

    applyRules c n
      | c == 1 = fromEnum $ n `elem` [2, 3]
      | c == 0 = fromEnum $ n == 3
      | otherwise = 0