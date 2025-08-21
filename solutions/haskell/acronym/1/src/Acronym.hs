module Acronym (abbreviate) where

import Data.Char (isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as T

abbreviate :: Text -> Text
abbreviate = T.toUpper . T.concat . map extractInitials . T.words . normalizeText
  where
    normalizeText :: Text -> Text
    normalizeText = T.map (\c -> if c == '-' || c == '_' then ' ' else c)

    extractInitials :: Text -> Text
    extractInitials w
      | T.all isAsciiUpper w = T.take 1 w
      | otherwise = T.cons (T.head w) (T.filter isAsciiUpper (T.tail w))