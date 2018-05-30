{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}
module Data.Abstract.Live where

import Data.Semilattice.Lower
import Data.Set as Set
import Prologue

-- | A set of live addresses (whether roots or reachable).
newtype Live location = Live { unLive :: Set location }
  deriving (Eq, Lower, Monoid, Ord, Semigroup)

fromAddresses :: (Foldable t, Ord location) => t location -> Live location
fromAddresses = Prologue.foldr liveInsert lowerBound

-- | Construct a 'Live' set containing only the given address.
liveSingleton :: location -> Live location
liveSingleton = Live . Set.singleton

-- | Insert an address into a 'Live' set.
liveInsert :: Ord location => location -> Live location -> Live location
liveInsert addr = Live . Set.insert addr . unLive

-- | Delete an address from a 'Live' set, if present.
liveDelete :: Ord location => location -> Live location -> Live location
liveDelete addr = Live . Set.delete addr . unLive

-- | Compute the (asymmetric) difference of two 'Live' sets, i.e. delete every element of the second set from the first set.
liveDifference :: Ord location => Live location -> Live location -> Live location
liveDifference = fmap Live . (Set.difference `on` unLive)

-- | Test whether an 'Address' is in a 'Live' set.
liveMember :: Ord location => location -> Live location -> Bool
liveMember addr = Set.member addr . unLive

-- | Decompose a 'Live' set into a pair of one member address and the remaining set, or 'Nothing' if empty.
liveSplit :: Live location -> Maybe (location, Live location)
liveSplit = fmap (fmap Live) . Set.minView . unLive


instance Show location => Show (Live location) where
  showsPrec d = showsUnaryWith showsPrec "Live" d . Set.toList . unLive
