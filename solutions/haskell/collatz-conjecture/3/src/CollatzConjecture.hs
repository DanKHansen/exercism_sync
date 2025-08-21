{-# LANGUAGE BangPatterns #-}

module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n =
  if n > 0 then Just (loop n 0) else Nothing
  where
    loop 1 !acc = acc
    loop i !acc = loop (if even i then div i 2 else 3 * i + 1) (acc + 1)
