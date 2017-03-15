{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- |
-- Source position and span information
-- Mostly taken from purescript's SourcePos definition.
--
module SourceSpan where

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.List.NonEmpty (nonEmpty)
import Data.Semigroup
import Data.These
import Prologue
import Test.LeanCheck

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
  } deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

displaySourcePos :: SourcePos -> Text
displaySourcePos SourcePos{..} =
  "line " <> show line <> ", column " <> show column

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
  } deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

displayStartEndPos :: SourceSpan -> Text
displayStartEndPos sp =
  displaySourcePos (spanStart sp) <> " - " <> displaySourcePos (spanEnd sp)

unionSourceSpansFrom :: Foldable f => SourceSpan -> f SourceSpan -> SourceSpan
unionSourceSpansFrom sourceSpan = maybe sourceSpan sconcat . nonEmpty . toList

unionSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
unionSourceSpan (SourceSpan start1 end1) (SourceSpan start2 end2) = SourceSpan (min start1 start2) (max end1 end2)

emptySourceSpan :: SourceSpan
emptySourceSpan = SourceSpan (SourcePos 1 1) (SourcePos 1 1)

instance Semigroup SourceSpan where
  a <> b = unionSourceSpan a b

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

instance Listable SourcePos where
  tiers = cons2 SourcePos

instance Listable SourceSpan where
  tiers = cons2 SourceSpan
