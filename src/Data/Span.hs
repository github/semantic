{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Source position and span information
--
--   Mostly taken from purescript's SourcePos definition.
module Data.Span
( Span(..)
, HasSpan(..)
, Pos(..)
, line
, column
, spanFromSrcLoc
, emptySpan
) where

import Prelude hiding (span)
import Prologue

import           Control.Lens.Lens
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import           Proto3.Suite

-- | Source position information (1 indexed)
data Pos = Pos
  { posLine   :: !Int
  , posColumn :: !Int
  } deriving (Eq, Ord, Generic, Hashable, NFData)

line, column :: Lens' Pos Int
line   = lens posLine   (\p l -> p { posLine   = l })
column = lens posColumn (\p l -> p { posColumn = l })

-- | A Span of position information
data Span = Span
  { spanStart :: Pos
  , spanEnd   :: Pos
  } deriving (Eq, Ord, Generic, Hashable, Named, NFData)

-- | "Classy-fields" interface for data types that have spans.
class HasSpan a where
  span  :: Lens' a Span

  start :: Lens' a Pos
  start = span.start
  {-# INLINE start #-}

  end :: Lens' a Pos
  end = span.end
  {-# INLINE end #-}

instance HasSpan Span where
  span  = id
  {-# INLINE span #-}

  start = lens spanStart (\s t -> s { spanStart = t })
  {-# INLINE start #-}

  end = lens spanEnd   (\s t -> s { spanEnd   = t })
  {-# INLINE end #-}

-- Instances

instance Show Pos where
  showsPrec _ Pos{..} = showChar '[' . shows posLine . showString ", " . shows posColumn . showChar ']'

instance A.ToJSON Pos where
  toJSON Pos{..} =
    A.toJSON [posLine, posColumn]

instance A.FromJSON Pos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    pure $ Pos line col

instance Lower Pos where
  lowerBound = Pos 1 1

instance Show Span where
  showsPrec _ Span{..} = shows spanStart . showString ".." . shows spanEnd

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

instance Lower Span where
  lowerBound = emptySpan
