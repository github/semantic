{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Control.Abstract.Heap
( Heap
, Addressable(..)
, getHeap
, putHeap
, modifyHeap
, lookupHeap
, assign
) where

import Control.Abstract.Context
import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Monoid (Last(..))
import Data.Semigroup.Reducer
import Prologue

-- | Retrieve the heap.
getHeap :: Member (State (Heap location (Cell location) value)) effects => Evaluator location value effects (Heap location (Cell location) value)
getHeap = get

-- | Set the heap.
putHeap :: Member (State (Heap location (Cell location) value)) effects => Heap location (Cell location) value -> Evaluator location value effects ()
putHeap = put

-- | Update the heap.
modifyHeap :: Member (State (Heap location (Cell location) value)) effects => (Heap location (Cell location) value -> Heap location (Cell location) value) -> Evaluator location value effects ()
modifyHeap = modify'

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (Member (State (Heap location (Cell location) value)) effects, Ord location) => Address location value -> Evaluator location value effects (Maybe (Cell location value))
lookupHeap = flip fmap getHeap . heapLookup

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Member (State (Heap location (Cell location) value)) effects
          , Ord location
          , Reducer value (Cell location value)
          )
       => Address location value
       -> value
       -> Evaluator location value effects ()
assign address = modifyHeap . heapInsert address


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
