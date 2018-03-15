{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Analysis
import Control.Applicative
import Control.Monad ((<=<))
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Abstract.Value
import Data.Monoid (Alt(..))
import Data.Semigroup
import Data.Semigroup.Reducer
import Prelude hiding (fail)
import Prologue

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class (Monad m, Ord l, l ~ LocationFor value, Reducer value (Cell l value)) => MonadAddressable l value m where
  deref :: Address l value -> m value

  alloc :: Name -> m (Address l value)

-- | Look up or allocate an address for a 'Name' free in a given term & assign it a given value, returning the 'Name' paired with the address.
--
--   The term is expected to contain one and only one free 'Name', meaning that care should be taken to apply this only to e.g. identifiers.
lookupOrAlloc :: ( FreeVariables term
                 , MonadAddressable (LocationFor value) value m
                 , MonadHeap value m
                 , Semigroup (CellFor value)
                 )
                 => term
                 -> value
                 -> EnvironmentFor value
                 -> m (Name, Address (LocationFor value) value)
lookupOrAlloc term = let [name] = toList (freeVariables term) in
                         lookupOrAlloc' name

-- | Look up or allocate an address for a 'Name' & assign it a given value, returning the 'Name' paired with the address.
lookupOrAlloc' :: ( Semigroup (CellFor value)
                  , MonadAddressable (LocationFor value) value m
                  , MonadHeap value m
                  )
                  => Name
                  -> value
                  -> EnvironmentFor value
                  -> m (Name, Address (LocationFor value) value)
lookupOrAlloc' name v env = do
  a <- maybe (alloc name) pure (envLookup name env)
  assign a v
  pure (name, a)


letrec :: ( MonadAddressable (LocationFor value) value m
          , MonadEnvironment value m
          , MonadHeap value m
          )
       => Name
       -> m value
       -> m (value, Address (LocationFor value) value)
letrec name body = do
  addr <- alloc name
  v <- localEnv (envInsert name addr) body
  assign addr v
  pure (v, addr)


-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance (MonadFail m, LocationFor value ~ Precise, MonadHeap value m) => MonadAddressable Precise value m where
  deref = maybe uninitializedAddress (pure . unLatest) <=< lookupHeap

  alloc _ = fmap (Address . Precise . heapSize) getHeap


-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative m, LocationFor value ~ Monovariant, MonadFail m, MonadHeap value m, Ord value) => MonadAddressable Monovariant value m where
  deref = maybe uninitializedAddress (foldMapA pure) <=< lookupHeap

  alloc = pure . Address . Monovariant

-- | Fold a collection by mapping each element onto an 'Alternative' action.
foldMapA :: (Alternative m, Foldable t) => (b -> m a) -> t b -> m a
foldMapA f = getAlt . foldMap (Alt . f)

-- | Fail with a message denoting an uninitialized address (i.e. one which was 'alloc'ated, but never 'assign'ed a value before being 'deref'erenced).
uninitializedAddress :: MonadFail m => m a
uninitializedAddress = fail "uninitialized address"
