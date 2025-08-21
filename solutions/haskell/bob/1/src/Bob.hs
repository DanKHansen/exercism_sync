{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Bob (responseFor) where

import Data.Char (isLetter, isLower)
import Data.Text (Text, strip, unsnoc)
import qualified Data.Text as Text

responseFor :: Text -> Text
responseFor (strip -> query)
  | Text.null query = "Fine. Be that way!"
  | (snd <$> unsnoc query) == Just '?' && isYelled = "Calm down, I know what I'm doing!"
  | (snd <$> unsnoc query) == Just '?' = "Sure."
  | isYelled = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    isYelled = Text.any isLetter query && not (Text.any isLower query)