module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate l =
  [ [ if (l !! r) !! c == '*'
        then '*'
        else
          let n =
                length
                  [ (x, y)
                    | dx <- [-1 .. 1],
                      let x = r + dx,
                      dy <- [-1 .. 1],
                      let y = c + dy,
                      (x /= r || y /= c)
                        && x >= 0
                        && x < length l
                        && y >= 0
                        && y < length (head l),
                      (l !! x) !! y == '*'
                  ]
           in if n == 0 then ' ' else head (show n)
      | c <- [0 .. (length (l !! r) - 1)]
    ]
    | r <- [0 .. (length l - 1)]
  ]
