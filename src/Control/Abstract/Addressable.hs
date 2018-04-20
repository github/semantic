{-# LANGUAGE TypeFamilies, UndecidableInstances, GADTs #-}
module Control.Abstract.Addressable where

import Control.Abstract.Evaluator
import Control.Applicative
import Control.Effect.Fresh
import Data.Abstract.Address
import Data.Abstract.Environment (insert)
import Data.Abstract.FreeVariables
import Data.Semigroup.Reducer
import Prelude hiding (fail)
import Prologue

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class (MonadFresh m, Ord location) => MonadAddressable location m where
  derefCell :: Address location value -> Cell location value -> m value

  allocLoc :: Name -> m location

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( MonadAddressable location m
                 , MonadEnvironment location value m
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
instance (MonadFail m, MonadFresh m) => MonadAddressable Precise m where
  derefCell addr = maybeM (uninitializedAddress addr) . unLatest
  allocLoc _ = Precise <$> fresh

-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative m, MonadFresh m) => MonadAddressable Monovariant m where
  derefCell _ = foldMapA pure
  allocLoc = pure . Monovariant

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: (MonadThrow (AddressError location value) m, MonadAddressable location m, MonadHeap location value m) => Address location value -> m value
deref addr = lookupHeap addr >>= maybe (throwAddressError $ UninitializedAddress addr) (derefCell addr)

alloc :: MonadAddressable location m => Name -> m (Address location value)
alloc = fmap Address . allocLoc

-- | Fail with a message denoting an uninitialized address (i.e. one which was 'alloc'ated, but never 'assign'ed a value before being 'deref'erenced).
uninitializedAddress :: (MonadFail m, Show location) => Address location value -> m a
uninitializedAddress addr = fail $ "uninitialized address: " <> show addr

data AddressError location value resume where
  UninitializedAddress :: Address location value -> AddressError location value value

deriving instance Eq location => Eq (AddressError location value resume)
deriving instance Show location => Show (AddressError location value resume)
instance Show location => Show1 (AddressError location value) where
  liftShowsPrec _ _ = showsPrec
instance Eq location => Eq1 (AddressError location value) where
  liftEq _ (UninitializedAddress a) (UninitializedAddress b)     = a == b


throwAddressError :: (MonadThrow (AddressError location value) m) => AddressError location value resume -> m resume
throwAddressError = throwResumable

