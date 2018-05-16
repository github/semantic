{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Address.Precise where

import Data.Monoid (Last(..))
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Prologue

-- | 'Precise' models precise store semantics where only the 'Latest' value is taken. Everything gets it's own address (always makes a new allocation) which makes for a larger store.
newtype Precise = Precise { unPrecise :: Int }
  deriving (Eq, Ord)

instance Show Precise where
  showsPrec d = showsUnaryWith showsPrec "Precise" d . unPrecise


-- | A cell holding a single value. Writes will replace any prior value.
--
--   This is equivalent to 'Data.Monoid.Last', but with a 'Show' instance designed to minimize the amount of text we have to scroll past in ghci.
newtype Latest value = Latest { unLatest :: Last value }
  deriving (Eq, Foldable, Functor, Lower, Monoid, Semigroup, Ord, Traversable)

instance Reducer value (Latest value) where
  unit = Latest . unit . Just

instance Show value => Show (Latest value) where
  showsPrec d = showsPrec d . getLast . unLatest
