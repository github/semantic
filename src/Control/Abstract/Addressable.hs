{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Analysis
import Control.Applicative
import Control.Monad ((<=<))
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Foldable (asum, toList)
import Data.Semigroup.Reducer
import Prelude hiding (fail)

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Store.
class (Monad m, Ord l, l ~ LocationFor value, Reducer value (Cell l value)) => MonadAddressable l value m where
  deref :: Address l value -> m value

  alloc :: Name -> m (Address l value)

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( MonadAddressable (LocationFor value) value m
                 , MonadEnvironment value m
                 )
                 => Name
                 -> m (Address (LocationFor value) value)
lookupOrAlloc name = lookupLocalEnv name >>= maybe (alloc name) pure


letrec :: ( MonadAddressable (LocationFor value) value m
          , MonadEnvironment value m
          , MonadStore value m
          )
       => Name
       -> m value
       -> m (value, Address (LocationFor value) value)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- localEnv (envInsert name addr) body
  assign addr v
  pure (v, addr)


-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance (MonadFail m, LocationFor value ~ Precise, MonadStore value m) => MonadAddressable Precise value m where
  deref = maybe uninitializedAddress (pure . unLatest) <=< flip fmap getStore . storeLookup
    where
      -- | Fail with a message denoting an uninitialized address (i.e. one which was 'alloc'ated, but never 'assign'ed a value before being 'deref'erenced).
      uninitializedAddress :: MonadFail m => m a
      uninitializedAddress = fail "uninitialized address"

  alloc _ = fmap (Address . Precise . storeSize) getStore


-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative m, LocationFor value ~ Monovariant, MonadStore value m, Ord value) => MonadAddressable Monovariant value m where
  deref = asum . maybe [] (map pure . toList) <=< flip fmap getStore . storeLookup

  alloc = pure . Address . Monovariant
