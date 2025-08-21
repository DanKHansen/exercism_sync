{-# LANGUAGE LambdaCase #-}

module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA =
  traverse
    ( \case
        'C' -> Right 'G'
        'G' -> Right 'C'
        'T' -> Right 'A'
        'A' -> Right 'U'
        c -> Left c
    )
