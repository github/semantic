{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.ScopeGraph
  ( Scope
  , scopeLookup
  , scopeLookupAll
  , scopeInsert
  , scopeDelete
  , scopeInit
  , scopeSize
  , scopeRestrict
  ) where

import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Prologue

-- | A map of addresses onto cells holding their values.
newtype Scope address value = Scope { unScope :: Monoidal.Map address (address, Set value) }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Semigroup)

-- | Look up the cell of values for an 'Address' in a 'Scope', if any.
scopeLookup :: Ord address => address -> Scope address value -> Maybe (Set value)
scopeLookup address = Monoidal.lookup address . unScope

-- | Look up the list of values stored for a given address, if any.
scopeLookupAll :: Ord address => address -> Scope address value -> Maybe [value]
scopeLookupAll address = fmap toList . scopeLookup address

-- | Append a value onto the cell for a given address, inserting a new cell if none existed.
scopeInsert :: (Ord address, Ord value) => address -> value -> Scope address value -> Scope address value
scopeInsert address value = flip snoc (address, value)

-- | Manually insert a cell into the scope at a given address.
scopeInit :: Ord address => address -> Set value -> Scope address value -> Scope address value
scopeInit address cell (Scope h) = Scope (Monoidal.insert address cell h)

-- | The number of addresses extant in a 'Scope'.
scopeSize :: Scope address value -> Int
scopeSize = Monoidal.size . unScope

-- | Restrict a 'Scope' to only those addresses in the given 'Live' set (in essence garbage collecting the rest).
scopeRestrict :: Ord address => Scope address value -> Live address -> Scope address value
scopeRestrict (Scope m) roots = Scope (Monoidal.filterWithKey (\ address _ -> address `liveMember` roots) m)

scopeDelete :: Ord address => address -> Scope address value -> Scope address value
scopeDelete addr = Scope . Monoidal.delete addr . unScope

instance (Ord address, Ord value) => Reducer (address, value) (Scope address value) where
  unit = Scope . unit
  cons (addr, a) (Scope scope) = Scope (cons (addr, a) scope)
  snoc (Scope scope) (addr, a) = Scope (snoc scope (addr, a))

instance (Show address, Show value) => Show (Scope address value) where
  showsPrec d = showsUnaryWith showsPrec "Scope" d . map (second toList) . Monoidal.pairs . unScope
