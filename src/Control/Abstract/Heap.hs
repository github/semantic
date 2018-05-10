module Control.Abstract.Heap
( Heap
, Cell
, getHeap
, putHeap
, modifyHeap
, lookupHeap
, assign
) where

import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Heap
import Data.Semigroup.Reducer
import Prologue

-- | Retrieve the heap.
getHeap :: Member (State (Heap (Cell location) location value)) effects => Evaluator location value effects (Heap (Cell location) location value)
getHeap = raise get

-- | Set the heap.
putHeap :: Member (State (Heap (Cell location) location value)) effects => Heap (Cell location) location value -> Evaluator location value effects ()
putHeap = raise . put

-- | Update the heap.
modifyHeap :: Member (State (Heap (Cell location) location value)) effects => (Heap (Cell location) location value -> Heap (Cell location) location value) -> Evaluator location value effects ()
modifyHeap = raise . modify'

-- | Look up the cell for the given 'Address' in the 'Heap'.
lookupHeap :: (Member (State (Heap (Cell location) location value)) effects, Ord location) => Address location value -> Evaluator location value effects (Maybe (Cell location value))
lookupHeap = flip fmap getHeap . heapLookup

-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Member (State (Heap (Cell location) location value)) effects
          , Ord location
          , Reducer value (Cell location value)
          )
       => Address location value
       -> value
       -> Evaluator location value effects ()
assign address = modifyHeap . heapInsert address
