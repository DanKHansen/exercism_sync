module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | all (`elem` "ACGT") xs = Right $ M.fromList $ map (\cs -> (fromJust $ toN $ head cs, length cs)) $ group $ sort xs
  | otherwise = Left "Error"

toN :: Char -> Maybe Nucleotide
toN c = case c of
  'A' -> Just A
  'C' -> Just C
  'G' -> Just G
  'T' -> Just T
  _ -> Nothing