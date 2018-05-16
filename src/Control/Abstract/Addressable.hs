{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Addressable where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Prologue

alloc :: Addressable location effects => Name -> Evaluator location value effects (Address location value)
alloc = fmap Address . allocCell

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: (Addressable location effects, Members '[Resumable (AddressError location value), State (Heap location (Cell location) value)] effects) => Address location value -> Evaluator location value effects value
deref addr = do
  cell <- lookupHeap addr >>= maybeM (throwAddressError (UnallocatedAddress addr))
  derefed <- derefCell addr cell
  maybeM (throwAddressError (UninitializedAddress addr)) derefed


-- | Defines allocation and dereferencing of 'Address'es in a 'Heap'.
class (Ord location, Show location) => Addressable location effects where
  -- | The type into which stored values will be written for a given location type.
  type family Cell location :: * -> *

  allocCell :: Name -> Evaluator location value effects location
  derefCell :: Address location value -> Cell location value -> Evaluator location value effects (Maybe value)


-- | 'Precise' locations are always allocated a fresh 'Address', and dereference to the 'Latest' value written.
instance Member Fresh effects => Addressable Precise effects where
  type Cell Precise = Latest

  allocCell _ = Precise <$> fresh
  derefCell _ = pure . getLast . unLatest

-- | 'Monovariant' locations allocate one 'Address' per unique variable name, and dereference once per stored value, nondeterministically.
instance Member NonDet effects => Addressable Monovariant effects where
  type Cell Monovariant = All

  allocCell = pure . Monovariant
  derefCell _ = traverse (foldMapA pure) . nonEmpty . toList

instance (Addressable location effects, Members '[Reader ModuleInfo, Reader PackageInfo] effects) => Addressable (Located location) effects where
  type Cell (Located location) = Cell location

  allocCell name = relocate (Located <$> allocCell name <*> currentPackage <*> currentModule)
  derefCell (Address (Located loc _ _)) = relocate . derefCell (Address loc)

relocate :: Evaluator location value effects a -> Evaluator (Located location) value effects a
relocate = raiseEff . lowerEff


-- Effects

data Allocator location value return where
  Alloc :: Name                   -> Allocator location value (Address location value)
  Deref :: Address location value -> Allocator location value value


runAllocator :: (Addressable location effects, Members '[Resumable (AddressError location value), State (Heap location (Cell location) value)] effects) => Evaluator location value (Allocator location value ': effects) a -> Evaluator location value effects a
runAllocator = interpret (\ eff -> case eff of
  Alloc name -> Address <$> allocCell name
  Deref addr -> lookupHeap addr >>= maybeM (throwAddressError (UnallocatedAddress addr)) >>= derefCell addr >>= maybeM (throwAddressError (UninitializedAddress addr)))

runAllocatorPrecise :: Members '[Fresh, Resumable (AddressError Precise value), State (Heap Precise Latest value)] effects => Evaluator Precise value (Allocator Precise value ': effects) a -> Evaluator Precise value effects a
runAllocatorPrecise = interpret (\ eff -> case eff of
  Alloc _    -> Address . Precise <$> fresh
  Deref addr -> lookupHeap addr >>= maybeM (throwAddressError (UnallocatedAddress addr)) >>= maybeM (throwAddressError (UninitializedAddress addr)) . getLast . unLatest)

runAllocatorMonovariant :: Members '[NonDet, Resumable (AddressError Monovariant value), State (Heap Monovariant All value)] effects => Evaluator Monovariant value (Allocator Monovariant value ': effects) a -> Evaluator Monovariant value effects a
runAllocatorMonovariant = interpret (\ eff -> case eff of
  Alloc name -> pure (Address (Monovariant name))
  Deref addr -> lookupHeap addr >>= maybeM (throwAddressError (UnallocatedAddress addr)) >>= traverse (foldMapA pure) . nonEmpty . toList >>= maybeM (throwAddressError (UninitializedAddress addr)))

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (Member (State (Heap location (Cell location) value)) effects, Ord location) => Address location value -> Evaluator location value effects (Maybe (Cell location value))
lookupHeap = flip fmap get . heapLookup


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


throwAddressError :: Member (Resumable (AddressError location value)) effects => AddressError location value resume -> Evaluator location value effects resume
throwAddressError = throwResumable

runAddressError :: Effectful (m location value) => m location value (Resumable (AddressError location value) ': effects) a -> m location value effects (Either (SomeExc (AddressError location value)) a)
runAddressError = runResumable

runAddressErrorWith :: Effectful (m location value) => (forall resume . AddressError location value resume -> m location value effects resume) -> m location value (Resumable (AddressError location value) ': effects) a -> m location value effects a
runAddressErrorWith = runResumableWith
