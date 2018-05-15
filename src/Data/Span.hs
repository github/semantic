{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Source position and span information
--
--   Mostly taken from purescript's SourcePos definition.
module Data.Span
( Span(..)
, Pos(..)
, spanFromSrcLoc
, emptySpan
) where

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.JSON.Fields
import Data.Semilattice.Lower
import GHC.Stack
import Prologue

-- | Source position information
data Pos = Pos
  { posLine   :: !Int
  , posColumn :: !Int
  }
  deriving (Eq, Ord)

instance A.ToJSON Pos where
  toJSON Pos{..} =
    A.toJSON [posLine, posColumn]

instance A.FromJSON Pos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    pure $ Pos line col

instance Show Pos where
  showsPrec d Pos{..} = showsBinaryWith showsPrec showsPrec "Pos" d posLine posColumn


data Span = Span
  { spanStart :: Pos
  , spanEnd   :: Pos
  }
  deriving (Show, Eq, Ord)

spanFromSrcLoc :: SrcLoc -> Span
spanFromSrcLoc = Span . (Pos . srcLocStartLine <*> srcLocStartCol) <*> (Pos . srcLocEndLine <*> srcLocEndCol)

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

instance ToJSONFields Span where
  toJSONFields sourceSpan = [ "sourceSpan" .= sourceSpan ]

instance Lower Span where
  lowerBound = Span (Pos 1 1) (Pos 1 1)
