module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = loop xs []
  where
    loop :: String -> String -> Either Char String
    loop s acc
      | null s = Right (reverse acc)
      | head s == 'C' = loop (tail s) ('G' : acc)
      | head s == 'G' = loop (tail s) ('C' : acc)
      | head s == 'T' = loop (tail s) ('A' : acc)
      | head s == 'A' = loop (tail s) ('U' : acc)
      | otherwise = Left (head s)
