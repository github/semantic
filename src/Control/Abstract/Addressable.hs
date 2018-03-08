{-# LANGUAGE FunctionalDependencies, TypeFamilies, UndecidableInstances #-}
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
class (Monad m, Ord l, l ~ LocationFor (ValueFor m), Reducer (ValueFor m) (Cell l (ValueFor m))) => MonadAddressable l m where
  deref :: Address l (ValueFor m)
        -> m (ValueFor m)

  alloc :: Name
        -> m (Address l (ValueFor m))

-- | Look up or allocate an address for a 'Name' free in a given term & assign it a given value, returning the 'Name' paired with the address.
--
--   The term is expected to contain one and only one free 'Name', meaning that care should be taken to apply this only to e.g. identifiers.
lookupOrAlloc :: ( FreeVariables t
                 , MonadAddressable (LocationFor a) m
                 , MonadEvaluator m
                 , a ~ ValueFor m
                 , Semigroup (CellFor a)
                 )
                 => t
                 -> a
                 -> Environment (LocationFor a) a
                 -> m (Name, Address (LocationFor a) a)
lookupOrAlloc term = let [name] = toList (freeVariables term) in
                         lookupOrAlloc' name
  where
    -- | Look up or allocate an address for a 'Name' & assign it a given value, returning the 'Name' paired with the address.
    lookupOrAlloc' :: ( Semigroup (CellFor a)
                      , MonadAddressable (LocationFor a) m
                      , a ~ ValueFor m
                      , MonadEvaluator m
                      )
                      => Name
                      -> a
                      -> Environment (LocationFor a) a
                      -> m (Name, Address (LocationFor a) a)
    lookupOrAlloc' name v env = do
      a <- maybe (alloc name) pure (envLookup name env)
      assign a v
      pure (name, a)

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Ord (LocationFor a)
          , MonadEvaluator m
          , a ~ ValueFor m
          , Reducer a (CellFor a)
          )
          => Address (LocationFor a) a
          -> a
          -> m ()
assign address = modifyStore . storeInsert address


-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance (Monad m, MonadEvaluator m, LocationFor (ValueFor m) ~ Precise) => MonadAddressable Precise m where
  deref = maybe uninitializedAddress (pure . unLatest) <=< flip fmap getStore . storeLookup
    where
      -- | Fail with a message denoting an uninitialized address (i.e. one which was 'alloc'ated, but never 'assign'ed a value before being 'deref'erenced).
      uninitializedAddress :: MonadFail m => m a
      uninitializedAddress = fail "uninitialized address"

  alloc _ = fmap (Address . Precise . storeSize) getStore


-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative m, Ord (ValueFor m), LocationFor (ValueFor m) ~ Monovariant, Monad m, MonadEvaluator m) => MonadAddressable Monovariant m where
  deref = asum . maybe [] (map pure . toList) <=< flip fmap getStore . storeLookup

  alloc = pure . Address . Monovariant
