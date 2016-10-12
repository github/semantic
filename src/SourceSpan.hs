{-# LANGUAGE DeriveAnyClass, StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- |
-- Source position and span information
-- Mostly taken from purescript's SourcePos definition.
--
module SourceSpan where

import Prologue
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Test.QuickCheck
import Data.These
import Data.Text.Arbitrary()

-- |
-- Source position information
--
data SourcePos = SourcePos
  { -- |
    -- Line number
    --
    line :: Int
    -- |
    -- Column number
    --
  , column :: Int
  } deriving (Show, Read, Eq, Ord, Generic, Hashable)

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
    -- Start of the span
    --
   spanStart :: SourcePos
    -- End of the span
    --
  , spanEnd :: SourcePos
  } deriving (Show, Read, Eq, Ord, Generic, Hashable)

displayStartEndPos :: SourceSpan -> Text
displayStartEndPos sp =
  displaySourcePos (spanStart sp) <> " - " <> displaySourcePos (spanEnd sp)

instance A.ToJSON SourceSpan where
  toJSON SourceSpan{..} =
    A.object [ "start" .= spanStart
             , "end" .= spanEnd
             ]

instance A.FromJSON SourceSpan where
  parseJSON = A.withObject "SourceSpan" $ \o ->
    SourceSpan <$>
      o .: "start" <*>
      o .: "end"


newtype SourceSpans = SourceSpans { unSourceSpans :: These SourceSpan SourceSpan }
  deriving (Eq, Show)

instance A.ToJSON SourceSpans where
  toJSON (SourceSpans spans) = case spans of
    (This span) -> A.object ["delete" .= span]
    (That span) -> A.object ["insert" .= span]
    (These span1 span2) -> A.object ["replace" .= (span1, span2)]
  toEncoding (SourceSpans spans) = case spans of
    (This span) -> A.pairs $ "delete" .= span
    (That span) -> A.pairs $ "insert" .= span
    (These span1 span2) -> A.pairs $ "replace" .= (span1, span2)

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary
  shrink = genericShrink
