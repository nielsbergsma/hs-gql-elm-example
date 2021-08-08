{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Commands
  ( NewPatient(..)
  , parseNewPatient
  ) where

import Prelude hiding (length)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.ICU (find, regex)
import qualified Data.Text as T
import qualified Schema as S

type Problem = String

data NewPatient = NewPatient
  { firstName :: Text
  , lastName  :: Text
  } deriving (Show)

parseNewPatient :: S.NewPatient -> Either Problem NewPatient
parseNewPatient (S.NewPatient {..}) = NewPatient 
  <$> (pure firstName >>= length 2 64 >>= pattern "^\\p{L}+[\\p{L}\\-']+$")
  <*> (pure lastName  >>= length 2 64 >>= pattern "^\\p{L}+[\\p{L}\\-']+$")

-- constraints
length :: Int -> Int -> Text -> Either Problem Text
length min max value
  | T.length value < min = Left "value too short"
  | T.length value > max = Left "value too long"
  | otherwise            = Right value

type Pattern = Text

pattern :: Pattern -> Text -> Either Problem Text
pattern pattern value
  | matches value = Right value
  | otherwise     = Left "invalid format"
  where matches = isJust <$> find (regex [] pattern)
