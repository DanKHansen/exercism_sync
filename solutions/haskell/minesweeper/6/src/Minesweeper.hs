module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board =
  [ [ if c == '*'
        then '*'
        else
          let n = length [() | dx <- [-1 .. 1], let x = r + dx, dy <- [-1 .. 1], let y = i + dy, x /= r || y /= i, inBounds x y, (board !! x) !! y == '*']
           in if n == 0 then ' ' else head (show n)
      | (i, c) <- zip [0 ..] row
    ]
    | (r, row) <- zip [0 ..] board
  ]
  where
    inBounds x y = x >= 0 && x < length board && y >= 0 && y < length (head board)