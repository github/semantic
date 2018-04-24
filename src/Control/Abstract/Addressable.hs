{-# LANGUAGE GADTs, TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Evaluator
import Control.Applicative
import Control.Effect
import Control.Effect.Fresh
import Control.Monad.Effect.Resumable as Eff
import Data.Abstract.Address
import Data.Abstract.Environment (insert)
import Data.Abstract.FreeVariables
import Data.Semigroup.Reducer
import Prelude hiding (fail)
import Prologue

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class (MonadFresh (m effects), Ord location) => MonadAddressable location (effects :: [* -> *]) m where
  derefCell :: Address location value -> Cell location value -> m effects value

  allocLoc :: Name -> m effects location

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( MonadAddressable location effects m
                 , MonadEnvironment location value effects m
                 )
              => Name
              -> m effects (Address location value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( MonadAddressable location effects m
          , MonadEnvironment location value effects m
          , MonadEvaluator location term value effects m
          , Reducer value (Cell location value)
          )
       => Name
       -> m effects value
       -> m effects (value, Address location value)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- localEnv (insert name addr) body
  assign addr v
  pure (v, addr)

-- Lookup/alloc a name passing the address to a body evaluated in a new local environment.
letrec' :: ( MonadAddressable location effects m
           , MonadEnvironment location value effects m
           )
        => Name
        -> (Address location value -> m effects value)
        -> m effects value
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- localEnv id (body addr)
  v <$ modifyEnv (insert name addr)

-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance (MonadFail (m effects), MonadFresh (m effects)) => MonadAddressable Precise effects m where
  derefCell addr = maybeM (uninitializedAddress addr) . unLatest
  allocLoc _ = Precise <$> fresh

-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative (m effects), MonadFresh (m effects)) => MonadAddressable Monovariant effects m where
  derefCell _ = foldMapA pure
  allocLoc = pure . Monovariant

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: (Member (Resumable (AddressError location value)) effects, MonadAddressable location effects m, MonadEvaluator location term value effects m) => Address location value -> m effects value
deref addr = lookupHeap addr >>= maybe (throwAddressError (UninitializedAddress addr)) (derefCell addr)

alloc :: MonadAddressable location effects m => Name -> m effects (Address location value)
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


throwAddressError :: (Effectful m, Member (Resumable (AddressError location value)) effects) => AddressError location value resume -> m effects resume
throwAddressError = raise . Eff.throwError
