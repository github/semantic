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
import Prologue

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class (Effectful m, Member Fresh effects, Monad (m effects), Ord location) => MonadAddressable location (effects :: [* -> *]) m where
  derefCell :: Address location value -> Cell location value -> m effects (Maybe value)

  allocLoc :: Name -> m effects location

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( MonadAddressable location effects m
                 , MonadEvaluator location term value effects m
                 )
              => Name
              -> m effects (Address location value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( MonadAddressable location effects m
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
           , MonadEvaluator location term value effects m
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
instance (Effectful m, Member Fresh effects, Monad (m effects)) => MonadAddressable Precise effects m where
  derefCell _ = pure . unLatest
  allocLoc _ = Precise <$> fresh

-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance (Alternative (m effects), Effectful m, Member Fresh effects, Monad (m effects)) => MonadAddressable Monovariant effects m where
  derefCell _ cell | null cell = pure Nothing
                   | otherwise = Just <$> foldMapA pure cell
  allocLoc = pure . Monovariant

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: (Member (Resumable (AddressError location value)) effects, MonadAddressable location effects m, MonadEvaluator location term value effects m) => Address location value -> m effects value
deref addr = do
  cell <- lookupHeap addr >>= maybeM (throwAddressError (UnallocatedAddress addr))
  derefed <- derefCell addr cell
  maybeM (throwAddressError (UninitializedAddress addr)) derefed

alloc :: MonadAddressable location effects m => Name -> m effects (Address location value)
alloc = fmap Address . allocLoc

data AddressError location value resume where
  UnallocatedAddress :: Address location value -> AddressError location value (Cell location value)
  UninitializedAddress :: Address location value -> AddressError location value value

deriving instance Eq location => Eq (AddressError location value resume)
deriving instance Show location => Show (AddressError location value resume)
instance Show location => Show1 (AddressError location value) where
  liftShowsPrec _ _ = showsPrec
instance Eq location => Eq1 (AddressError location value) where
  liftEq _ (UninitializedAddress a) (UninitializedAddress b) = a == b
  liftEq _ (UnallocatedAddress a)   (UnallocatedAddress b)   = a == b
  liftEq _ _                        _                        = False


throwAddressError :: (Effectful m, Member (Resumable (AddressError location value)) effects) => AddressError location value resume -> m effects resume
throwAddressError = raise . Eff.throwError
