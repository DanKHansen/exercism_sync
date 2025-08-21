module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
  let ds = dropWhile (== '1') $ filter isDigit xs
   in case ds of
        (d1 : _ : _ : d4 : _) | length ds == 10 && d1 > '1' && d4 > '1' -> Just ds
        _ -> Nothing