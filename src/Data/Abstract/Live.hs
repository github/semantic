{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}
module Data.Abstract.Live
  ( Live (..)
  , fromAddresses
  , liveSingleton
  , liveInsert
  , liveDelete
  , liveDifference
  , liveMember
  , liveSplit
  , liveMap
  ) where

import Data.Set as Set
import Prologue

-- | A set of live addresses (whether roots or reachable).
newtype Live address = Live { unLive :: Set address }
  deriving (Eq, Lower, Monoid, Ord, Semigroup)

fromAddresses :: (Foldable t, Ord address) => t address -> Live address
fromAddresses = Prologue.foldr liveInsert lowerBound

-- | Construct a 'Live' set containing only the given address.
liveSingleton :: address -> Live address
liveSingleton = Live . Set.singleton

-- | Insert an address into a 'Live' set.
liveInsert :: Ord address => address -> Live address -> Live address
liveInsert addr = Live . Set.insert addr . unLive

-- | Delete an address from a 'Live' set, if present.
liveDelete :: Ord address => address -> Live address -> Live address
liveDelete addr = Live . Set.delete addr . unLive

-- | Compute the (asymmetric) difference of two 'Live' sets, i.e. delete every element of the second set from the first set.
liveDifference :: Ord address => Live address -> Live address -> Live address
liveDifference = fmap Live . (Set.difference `on` unLive)

-- | Test whether an address is in a 'Live' set.
liveMember :: Ord address => address -> Live address -> Bool
liveMember addr = Set.member addr . unLive

-- | Decompose a 'Live' set into a pair of one member address and the remaining set, or 'Nothing' if empty.
liveSplit :: Live address -> Maybe (address, Live address)
liveSplit = fmap (fmap Live) . Set.minView . unLive

-- | Map a function over the addresses in a 'Live' set.
liveMap :: Ord b => (a -> b) -> Live a -> Live b
liveMap f = Live . Set.map f . unLive


instance Show address => Show (Live address) where
  showsPrec d = showsUnaryWith showsPrec "Live" d . Set.toList . unLive
