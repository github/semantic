{-# LANGUAGE DeriveGeneric, DerivingVia #-}
module Source.Loc
( Loc(..)
, Span(Span)
, Range(Range)
) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Monoid.Generic
import GHC.Generics (Generic)
import Source.Range
import Source.Span

data Loc = Loc
  { locByteRange :: {-# UNPACK #-} !Range
  , locSpan      :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Ord, Show, Generic)
  deriving Semigroup via GenericSemigroup Loc

instance Hashable Loc
instance NFData   Loc

instance HasSpan Loc where
  span_ = lens locSpan (\l s -> l { locSpan = s }) where
    lens get put afa s = fmap (put s) (afa (get s))
  {-# INLINE span_ #-}
