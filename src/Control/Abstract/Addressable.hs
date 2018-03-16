{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Analysis
import Control.Applicative
import Control.Monad ((<=<))
import Data.Abstract.Address
import Data.Abstract.Environment (insert)
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Abstract.Value
import Data.Semigroup.Reducer
import Prelude hiding (fail)
import Prologue

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class (Monad m, Ord l, l ~ LocationFor value, Reducer value (Cell l value)) => MonadAddressable l value m where
  deref :: Address l value -> m value

  alloc :: Name -> m (Address l value)

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( MonadAddressable (LocationFor value) value m
                 , MonadEnvironment value m
                 )
                 => Name
                 -> m (Address (LocationFor value) value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( MonadAddressable (LocationFor value) value m
          , MonadEnvironment value m
          , MonadHeap value m
          )
       => Name
       -> m value
       -> m (value, Address (LocationFor value) value)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- localEnv (insert name addr) body
  assign addr v
  pure (v, addr)


-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance (MonadFail m, LocationFor value ~ Precise, MonadHeap value m) => MonadAddressable Precise value m where
  deref = derefWith (maybeM uninitializedAddress . unLatest)
  alloc _ = do
    -- Compute the next available address in the heap, then write an empty value into it.
    addr <- fmap (Address . Precise . heapSize) getHeap
    addr <$ modifyHeap (heapInit addr mempty)

-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative m, LocationFor value ~ Monovariant, MonadFail m, MonadHeap value m, Ord value) => MonadAddressable Monovariant value m where
  deref = derefWith (foldMapA pure)
  alloc = pure . Address . Monovariant

-- | Dereference the given 'Address' in the heap, using the supplied function to act on the cell, or failing if the address is uninitialized.
derefWith :: (MonadFail m, MonadHeap value m, Ord (LocationFor value)) => (CellFor value -> m a) -> Address (LocationFor value) value -> m a
derefWith with = maybe uninitializedAddress with <=< lookupHeap

-- | Fail with a message denoting an uninitialized address (i.e. one which was 'alloc'ated, but never 'assign'ed a value before being 'deref'erenced).
uninitializedAddress :: MonadFail m => m a
uninitializedAddress = fail "uninitialized address"
