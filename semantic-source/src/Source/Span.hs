{-# LANGUAGE DeriveGeneric, OverloadedStrings, RankNTypes #-}
-- | Source position and span information
--
--   Mostly taken from purescript's SourcePos definition.
module Source.Span
( Span(..)
, spanFromSrcLoc
, Pos(..)
, line_
, column_
, HasSpan(..)
) where

import           Control.DeepSeq (NFData)
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import           Data.Hashable (Hashable)
import           Data.Semilattice.Lower (Lower(..))
import           GHC.Generics (Generic)
import           GHC.Stack (SrcLoc(..))
import           Prelude hiding (span)

-- | A Span of position information
data Span = Span
  { spanStart :: {-# UNPACK #-} !Pos
  , spanEnd   :: {-# UNPACK #-} !Pos
  }
  deriving (Eq, Ord, Generic, Show)

instance Hashable Span
instance NFData   Span

instance Semigroup Span where
  Span start1 end1 <> Span start2 end2 = Span (min start1 start2) (max end1 end2)

instance A.ToJSON Span where
  toJSON s = A.object
    [ "start" .= spanStart s
    , "end"   .= spanEnd   s
    ]

instance A.FromJSON Span where
  parseJSON = A.withObject "Span" $ \o -> Span
    <$> o .: "start"
    <*> o .: "end"

instance Lower Span where
  lowerBound = Span lowerBound lowerBound


spanFromSrcLoc :: SrcLoc -> Span
spanFromSrcLoc s = Span (Pos (srcLocStartLine s) (srcLocStartCol s)) (Pos (srcLocEndLine s) (srcLocEndCol s))


-- | Source position information (1-indexed)
data Pos = Pos
  { line   :: {-# UNPACK #-} !Int
  , column :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Generic, Show)

instance Hashable Pos
instance NFData   Pos

instance A.ToJSON Pos where
  toJSON p = A.toJSON
    [ line   p
    , column p
    ]

instance A.FromJSON Pos where
  parseJSON arr = do
    [ line, col ] <- A.parseJSON arr
    pure $ Pos line col

instance Lower Pos where
  lowerBound = Pos 1 1


line_, column_ :: Lens' Pos Int
line_   = lens line   (\p l -> p { line   = l })
column_ = lens column (\p l -> p { column = l })


-- | "Classy-fields" interface for data types that have spans.
class HasSpan a where
  span_ :: Lens' a Span

  start_ :: Lens' a Pos
  start_ = span_.start_
  {-# INLINE start_ #-}

  end_ :: Lens' a Pos
  end_ = span_.end_
  {-# INLINE end_ #-}

instance HasSpan Span where
  span_  = id
  {-# INLINE span_ #-}

  start_ = lens spanStart (\s t -> s { spanStart = t })
  {-# INLINE start_ #-}

  end_   = lens spanEnd   (\s t -> s { spanEnd   = t })
  {-# INLINE end_ #-}


type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens get put afa s = fmap (put s) (afa (get s))
{-# INLINE lens #-}
