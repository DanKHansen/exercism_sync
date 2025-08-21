module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)
import GHC.Float (sqrtDouble)

encode :: String -> String
encode xs = unwords . transpose . padTo r $ grouped r n
  where
    n = map toLower (filter isAlphaNum xs)
    len = length n
    r = ceiling $ sqrtDouble $ fromIntegral len

    grouped _ [] = []
    grouped i cs = take i cs : grouped i (drop i cs)

    padTo x = map (\cs -> cs ++ replicate (x - length cs) ' ')
