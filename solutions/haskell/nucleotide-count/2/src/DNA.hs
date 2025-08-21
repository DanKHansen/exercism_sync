module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | all (`elem` "ACGT") xs = Right $ M.fromListWith (+) [(n, 1) | c <- xs, Just n <- [toN c]]
  | otherwise = Left "Error"

toN :: Char -> Maybe Nucleotide
toN 'A' = Just A
toN 'C' = Just C
toN 'G' = Just G
toN 'T' = Just T
toN _ = Nothing