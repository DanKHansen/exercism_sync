module ReverseString (reverseString) where

reverseString :: String -> String
reverseString s = rev s []
  where
    rev [] a = a
    rev (x : xs) a = rev xs (x : a)
