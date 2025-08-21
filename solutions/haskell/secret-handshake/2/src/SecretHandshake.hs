module SecretHandshake (handshake) where

import Data.Bits (Bits ((.&.)), shiftL)
import Data.List (foldl')

actionMap :: [(Int, [String] -> [String])]
actionMap =
  [ (shiftL 1 0, \acc -> acc ++ ["wink"]),
    (shiftL 1 1, \acc -> acc ++ ["double blink"]),
    (shiftL 1 2, \acc -> acc ++ ["close your eyes"]),
    (shiftL 1 3, \acc -> acc ++ ["jump"]),
    (shiftL 1 4, reverse)
  ]

handshake :: Int -> [String]
handshake n = foldl' (\acc (mask, apply) -> if n .&. mask /= 0 then apply acc else acc) [] actionMap