module SecretHandshake (handshake) where

import Data.Bits (Bits ((.&.)), shiftL)

actionMap :: [(Int, [String] -> [String])]
actionMap =
  [ (shiftL 1 0, \acc -> acc ++ ["wink"]),
    (shiftL 1 1, \acc -> acc ++ ["double blink"]),
    (shiftL 1 2, \acc -> acc ++ ["close your eyes"]),
    (shiftL 1 3, \acc -> acc ++ ["jump"]),
    (shiftL 1 4, reverse)
  ]

handshake :: Int -> [String]
handshake n = go n actionMap []
  where
    go _ [] acc = acc
    go x ((mask, action) : rest) acc
      | x .&. mask /= 0 = go x rest (action acc)
      | otherwise = go x rest acc
