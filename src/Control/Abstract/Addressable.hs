{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Evaluator
import Control.Applicative
import Control.Monad ((<=<))
import Data.Abstract.Address
import Data.Abstract.Environment (insert)
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Semigroup.Reducer
import Prelude hiding (fail)
import Prologue

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class (Monad m, Ord location) => MonadAddressable location m where
  deref :: MonadHeap location value m => Address location value -> m value

  alloc :: MonadHeap location value m => Name -> m (Address location value)

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( MonadAddressable location m
                 , MonadEnvironment location value m
                 , MonadHeap location value m
                 )
              => Name
              -> m (Address location value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( MonadAddressable location m
          , MonadEnvironment location value m
          , MonadHeap location value m
          , Reducer value (Cell location value)
          )
       => Name
       -> m value
       -> m (value, Address location value)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- localEnv (insert name addr) body
  assign addr v
  pure (v, addr)

-- Lookup/alloc a name passing the address to a body evaluated in a new local environment.
letrec' :: ( MonadAddressable location m
           , MonadEnvironment location value m
           , MonadHeap location value m
           )
        => Name
        -> (Address location value -> m value)
        -> m value
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- localEnv id (body addr)
  v <$ modifyEnv (insert name addr)

-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance MonadFail m => MonadAddressable Precise m where
  deref = derefWith (maybeM uninitializedAddress . unLatest)
  alloc _ = do
    -- Compute the next available address in the heap, then write an empty value into it.
    addr <- fmap (Address . Precise . heapSize) getHeap
    addr <$ modifyHeap (heapInit addr mempty)

-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative m, MonadFail m) => MonadAddressable Monovariant m where
  deref = derefWith (foldMapA pure)
  alloc = pure . Address . Monovariant

-- | Dereference the given 'Address' in the heap, using the supplied function to act on the cell, or failing if the address is uninitialized.
derefWith :: (MonadFail m, MonadHeap location value m, Ord location) => (Cell location value -> m a) -> Address location value -> m a
derefWith with = maybe uninitializedAddress with <=< lookupHeap

-- | Fail with a message denoting an uninitialized address (i.e. one which was 'alloc'ated, but never 'assign'ed a value before being 'deref'erenced).
uninitializedAddress :: MonadFail m => m a
uninitializedAddress = fail "uninitialized address"
