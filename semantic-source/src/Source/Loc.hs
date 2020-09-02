{-# LANGUAGE DeriveGeneric, RankNTypes, NamedFieldPuns, OverloadedStrings #-}
module Source.Loc
( Loc(..)
, byteRange_
, Span(Span)
, Range(Range)
) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude hiding (span)
import Source.Range
import Source.Span

data Loc = Loc
  { byteRange :: {-# UNPACK #-} !Range
  , span      :: {-# UNPACK #-} !Span
  }
  deriving (Eq, Ord, Show, Generic)

instance Semigroup Loc where
  Loc b1 s1 <> Loc b2 s2 = Loc (b1 <> b2) (s1 <> s2)

instance Hashable Loc
instance NFData   Loc

instance HasSpan Loc where
  span_ = lens span (\l s -> l { span = s })
  {-# INLINE span_ #-}

instance ToJSON Loc where
  toJSON Loc{byteRange, span} = object ["sourceRange" .= byteRange
                                      , "sourceSpan" .= span]

byteRange_ :: Lens' Loc Range
byteRange_ = lens byteRange (\l r -> l { byteRange = r })


type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens get put afa s = fmap (put s) (afa (get s))
{-# INLINE lens #-}
