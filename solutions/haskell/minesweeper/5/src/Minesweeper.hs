module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate l =
  [ [ if c == '*'
        then '*'
        else
          let n =
                length
                  [ ()
                    | dx <- [-1 .. 1],
                      let x = r + dx,
                      dy <- [-1 .. 1],
                      let y = i + dy,
                      x /= r || y /= i,
                      inBounds x y,
                      (l !! x) !! y == '*'
                  ]
           in if n == 0 then ' ' else head (show n)
      | (i, c) <- zip [0 ..] row
    ]
    | (r, row) <- zip [0 ..] l
  ]
  where
    inBounds x y = x >= 0 && x < length l && y >= 0 && y < length (head l)