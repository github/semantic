{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Addressable
( alloc
, deref
, Addressable(..)
-- * Effects
, Allocator(..)
, runAllocator
, AddressError(..)
, runAddressError
, runAddressErrorWith
) where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Prologue

alloc :: Member (Allocator location value) effects => Name -> Evaluator location value effects (Address location value)
alloc = send . Alloc

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: Member (Allocator location value) effects => Address location value -> Evaluator location value effects value
deref = send . Deref


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

-- | 'Located' locations allocate & dereference using the underlying location, contextualizing locations with the current 'PackageInfo' & 'ModuleInfo'.
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
  Deref addr -> heapLookup addr <$> get >>= maybeM (throwResumable (UnallocatedAddress addr)) >>= derefCell addr >>= maybeM (throwResumable (UninitializedAddress addr)))


data AddressError location value resume where
  UnallocatedAddress   :: Address location value -> AddressError location value (Cell location value)
  UninitializedAddress :: Address location value -> AddressError location value value

deriving instance Eq location => Eq (AddressError location value resume)
deriving instance Show location => Show (AddressError location value resume)
instance Show location => Show1 (AddressError location value) where
  liftShowsPrec _ _ = showsPrec
instance Eq location => Eq1 (AddressError location value) where
  liftEq _ (UninitializedAddress a) (UninitializedAddress b) = a == b
  liftEq _ (UnallocatedAddress a)   (UnallocatedAddress b)   = a == b
  liftEq _ _                        _                        = False


runAddressError :: Effectful (m location value) => m location value (Resumable (AddressError location value) ': effects) a -> m location value effects (Either (SomeExc (AddressError location value)) a)
runAddressError = runResumable

runAddressErrorWith :: Effectful (m location value) => (forall resume . AddressError location value resume -> m location value effects resume) -> m location value (Resumable (AddressError location value) ': effects) a -> m location value effects a
runAddressErrorWith = runResumableWith
