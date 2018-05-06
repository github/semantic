{-# LANGUAGE GADTs, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Evaluator
import Control.Monad.Effect.Resumable as Eff
import Data.Abstract.Address
import Data.Abstract.Environment (insert)
import Data.Abstract.FreeVariables
import Data.Semigroup.Reducer
import Prologue

-- | Defines 'alloc'ation and 'deref'erencing of 'Address'es in a Heap.
class Ord location => Addressable location effects where
  derefCell :: Address location value -> Cell location value -> Evaluator location term value effects (Maybe value)

  allocLoc :: Name -> Evaluator location term value effects location

-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( Addressable location effects
                 , Members '[ Reader (Environment location value)
                            , State (Environment location value)
                            ] effects
                 )
              => Name
              -> Evaluator location term value effects (Address location value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( Addressable location effects
          , Members '[ Reader (Environment location value)
                     , State (Environment location value)
                     , State (Heap location value)
                     ] effects
          , Reducer value (Cell location value)
          )
       => Name
       -> Evaluator location term value effects value
       -> Evaluator location term value effects (value, Address location value)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- localEnv (insert name addr) body
  assign addr v
  pure (v, addr)

-- Lookup/alloc a name passing the address to a body evaluated in a new local environment.
letrec' :: ( Addressable location effects
           , Members '[ Reader (Environment location value)
                      , State (Environment location value)
                      ] effects
           )
        => Name
        -> (Address location value -> Evaluator location term value effects value)
        -> Evaluator location term value effects value
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- localEnv id (body addr)
  v <$ modifyEnv (insert name addr)

-- Instances

-- | 'Precise' locations are always 'alloc'ated a fresh 'Address', and 'deref'erence to the 'Latest' value written.
instance Member Fresh effects => Addressable Precise effects where
  derefCell _ = pure . unLatest
  allocLoc _ = Precise <$> raise fresh

-- | 'Monovariant' locations 'alloc'ate one 'Address' per unique variable name, and 'deref'erence once per stored value, nondeterministically.
instance Members '[Fresh, NonDet] effects => Addressable Monovariant effects where
  derefCell _ cell | null cell = pure Nothing
                   | otherwise = Just <$> foldMapA pure cell
  allocLoc = pure . Monovariant

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: (Addressable location effects, Members '[Resumable (AddressError location value), State (Heap location value)] effects) => Address location value -> Evaluator location term value effects value
deref addr = do
  cell <- lookupHeap addr >>= maybeM (throwAddressError (UnallocatedAddress addr))
  derefed <- derefCell addr cell
  maybeM (throwAddressError (UninitializedAddress addr)) derefed

alloc :: Addressable location effects => Name -> Evaluator location term value effects (Address location value)
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


throwAddressError :: Member (Resumable (AddressError location value)) effects => AddressError location value resume -> Evaluator location term value effects resume
throwAddressError = raise . Eff.throwError

runAddressError :: Evaluator location term value (Resumable (AddressError location value) ': effects) a -> Evaluator location term value effects (Either (SomeExc (AddressError location value)) a)
runAddressError = raiseHandler runError
