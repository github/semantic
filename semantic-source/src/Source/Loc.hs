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
import Prelude hiding (span)
import Source.Range
import Source.Span

data Loc = Loc
  { byteRange :: {-# UNPACK #-} !Range
  , span      :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Ord, Show, Generic)
  deriving Semigroup via GenericSemigroup Loc

instance Hashable Loc
instance NFData   Loc

instance HasSpan Loc where
  span_ = lens span (\l s -> l { span = s }) where
    lens get put afa s = fmap (put s) (afa (get s))
  {-# INLINE span_ #-}
