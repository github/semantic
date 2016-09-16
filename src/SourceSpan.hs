-- |
-- Source position and span information
-- Mostly taken from purescript's SourcePos definition.
--
module SourceSpan where

import Prologue
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Test.QuickCheck
import Data.Text.Arbitrary()

-- |
-- Source position information
--
data SourcePos = SourcePos
  { -- |
    -- Line number
    --
    line :: !Int
    -- |
    -- Column number
    --
  , column :: !Int
  } deriving (Show, Read, Eq, Ord, Generic)

displaySourcePos :: SourcePos -> Text
displaySourcePos sp =
  "line " <> show (line sp) <> ", column " <> show (column sp)

instance A.ToJSON SourcePos where
  toJSON SourcePos{..} =
    A.toJSON [line, column]

instance A.FromJSON SourcePos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    pure $ SourcePos line col

data SourceSpan = SourceSpan
  { -- |
    -- Source name
    --
    spanName :: !Text
    -- |
    -- Start of the span
    --
  , spanStart :: !SourcePos
    -- End of the span
    --
  , spanEnd :: !SourcePos
  } deriving (Show, Read, Eq, Ord, Generic)

displayStartEndPos :: SourceSpan -> Text
displayStartEndPos sp =
  displaySourcePos (spanStart sp) <> " - " <> displaySourcePos (spanEnd sp)

displaySourceSpan :: SourceSpan -> Text
displaySourceSpan sp =
  spanName sp <> " " <> displayStartEndPos sp

instance A.ToJSON SourceSpan where
  toJSON SourceSpan{..} =
    A.object [ "name"  .= spanName
             , "start" .= spanStart
             , "end"   .= spanEnd
             ]

instance A.FromJSON SourceSpan where
  parseJSON = A.withObject "SourceSpan" $ \o ->
    SourceSpan     <$>
      o .: "name"  <*>
      o .: "start" <*>
      o .: "end"

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink
