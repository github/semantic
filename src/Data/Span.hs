{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Source position and span information
--
--   Mostly taken from purescript's SourcePos definition.
module Data.Span
( Span(..)
, Pos(..)
, emptySpan
) where

import Control.DeepSeq
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.Hashable (Hashable)
import Data.Semigroup
import GHC.Generics
import Test.LeanCheck

-- | Source position information
data Pos = Pos
  { posLine :: !Int
  , posColumn :: !Int
  }
  deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

instance A.ToJSON Pos where
  toJSON Pos{..} =
    A.toJSON [posLine, posColumn]

instance A.FromJSON Pos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    pure $ Pos line col

data Span = Span
  { spanStart :: Pos
  , spanEnd :: Pos
  }
  deriving (Show, Read, Eq, Ord, Generic, Hashable, NFData)

emptySpan :: Span
emptySpan = Span (Pos 1 1) (Pos 1 1)

instance Semigroup Span where
  Span start1 end1 <> Span start2 end2 = Span (min start1 start2) (max end1 end2)

instance A.ToJSON Span where
  toJSON Span{..} =
    A.object [ "start" .= spanStart
             , "end" .= spanEnd
             ]

instance A.FromJSON Span where
  parseJSON = A.withObject "Span" $ \o ->
    Span <$>
      o .: "start" <*>
      o .: "end"

instance Listable Pos where
  tiers = cons2 Pos

instance Listable Span where
  tiers = cons2 Span
