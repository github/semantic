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
import Data.Semigroup
import Data.Semigroup.Reducer
import Prelude hiding (fail)

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Store.
class (Monad m, Ord l, l ~ LocationFor value, Reducer value (Cell l value)) => MonadAddressable l value m where
  deref :: Address l value -> m value

  alloc :: Name -> m (Address l value)

-- | Look up or allocate an address for a 'Name' free in a given term & assign it a given value, returning the 'Name' paired with the address.
--
--   The term is expected to contain one and only one free 'Name', meaning that care should be taken to apply this only to e.g. identifiers.
lookupOrAlloc :: ( FreeVariables term
                 , MonadAddressable (LocationFor value) value m
                 , MonadEvaluator term value m
                 , Semigroup (CellFor value)
                 )
                 => term
                 -> value
                 -> Environment (LocationFor value) value
                 -> m (Name, Address (LocationFor value) value)
lookupOrAlloc term = let [name] = toList (freeVariables term) in
                         lookupOrAlloc' name

-- | Look up or allocate an address for a 'Name' & assign it a given value, returning the 'Name' paired with the address.
lookupOrAlloc' :: ( Semigroup (CellFor value)
                  , MonadAddressable (LocationFor value) value m
                  , MonadEvaluator term value m
                  )
                  => Name
                  -> value
                  -> Environment (LocationFor value) value
                  -> m (Name, Address (LocationFor value) value)
lookupOrAlloc' name v env = do
  a <- maybe (alloc name) pure (envLookup name env)
  assign a v
  pure (name, a)

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Ord (LocationFor value)
          , MonadEvaluator term value m
          , Reducer value (CellFor value)
          )
          => Address (LocationFor value) value
          -> value
          -> m ()
assign address = modifyStore . storeInsert address


-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance (Monad m, LocationFor value ~ Precise, MonadEvaluator term value m) => MonadAddressable Precise value m where
  deref = maybe uninitializedAddress (pure . unLatest) <=< flip fmap getStore . storeLookup
    where
      -- | Fail with a message denoting an uninitialized address (i.e. one which was 'alloc'ated, but never 'assign'ed a value before being 'deref'erenced).
      uninitializedAddress :: MonadFail m => m a
      uninitializedAddress = fail "uninitialized address"

  alloc _ = fmap (Address . Precise . storeSize) getStore


-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative m, Monad m, LocationFor value ~ Monovariant, MonadEvaluator term value m, Ord value) => MonadAddressable Monovariant value m where
  deref = asum . maybe [] (map pure . toList) <=< flip fmap getStore . storeLookup

  alloc = pure . Address . Monovariant
