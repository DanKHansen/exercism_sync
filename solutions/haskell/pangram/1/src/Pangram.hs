module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram str = all (`elem` map toLower str) ['a' .. 'z']