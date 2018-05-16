{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Address.Monovariant where

import Data.Abstract.FreeVariables (Name(..))
import qualified Data.Abstract.Heap as Heap
import qualified Data.Abstract.Live as Live
import Data.Semigroup.Reducer
import Data.Semilattice.Lower
import Data.Set as Set
import Prologue

-- | 'Monovariant' models using one address for a particular name. It trackes the set of values that a particular address takes and uses it's name to lookup in the store and only allocation if new.
newtype Monovariant = Monovariant { unMonovariant :: Name }
  deriving (Eq, Ord)

instance Show Monovariant where
  showsPrec d = showsUnaryWith showsPrec "Monovariant" d . unName . unMonovariant


-- | A cell holding all values written to its address.
--
--   This is equivalent to 'Set', but with a 'Show' instance designed to minimize the amount of text we have to scroll past in ghci.
newtype All value = All { unAll :: Set value }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Reducer value, Semigroup)

instance Show value => Show (All value) where
  showsPrec d = showsPrec d . Set.toList . unAll


type Heap = Heap.Heap Monovariant All
type Live = Live.Live Monovariant


-- | A single point in a program’s execution.
data Configuration term value = Configuration
  { configurationTerm  :: term       -- ^ The “instruction,” i.e. the current term to evaluate.
  , configurationRoots :: Live value -- ^ The set of rooted addresses.
  , configurationHeap  :: Heap value -- ^ The heap of values.
  }
  deriving (Eq, Ord, Show)
