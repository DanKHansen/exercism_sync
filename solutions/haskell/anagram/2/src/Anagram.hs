module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\x -> (sort (map toLower x) == sort (map toLower xs)) && map toLower xs /= map toLower x)