module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | all (`elem` "ACGT") xs = Right $ foldr countNucleotide M.empty xs
  | otherwise = Left "Error"

countNucleotide :: Char -> Map Nucleotide Int -> Map Nucleotide Int
countNucleotide c acc = case toN c of
  Just n -> M.insertWith (+) n 1 acc
  Nothing -> acc -- This case should not happen due to the earlier check

toN :: Char -> Maybe Nucleotide
toN 'A' = Just A
toN 'C' = Just C
toN 'G' = Just G
toN 'T' = Just T
toN _ = Nothing