{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Heap
( Heap
, getHeap
, putHeap
, modifyHeap
, assign
, lookupOrAlloc
, letrec
, letrec'
, variable
) where

import Control.Abstract.Addressable
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Heap
import Data.Semigroup.Reducer

-- | Retrieve the heap.
getHeap :: Member (State (Heap location (Cell location) value)) effects => Evaluator location value effects (Heap location (Cell location) value)
getHeap = get

-- | Set the heap.
putHeap :: Member (State (Heap location (Cell location) value)) effects => Heap location (Cell location) value -> Evaluator location value effects ()
putHeap = put

-- | Update the heap.
modifyHeap :: Member (State (Heap location (Cell location) value)) effects => (Heap location (Cell location) value -> Heap location (Cell location) value) -> Evaluator location value effects ()
modifyHeap = modify'


-- | Write a value to the given 'Address' in the 'Store'.
assign :: ( Member (State (Heap location (Cell location) value)) effects
          , Ord location
          , Reducer value (Cell location value)
          )
       => Address location value
       -> value
       -> Evaluator location value effects ()
assign address = modifyHeap . heapInsert address


-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( Addressable location effects
                 , Members '[ Reader (Environment location value)
                            , State (Environment location value)
                            ] effects
                 )
              => Name
              -> Evaluator location value effects (Address location value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( Addressable location effects
          , Members '[ Reader (Environment location value)
                     , State (Environment location value)
                     , State (Heap location (Cell location) value)
                     ] effects
          , Reducer value (Cell location value)
          )
       => Name
       -> Evaluator location value effects value
       -> Evaluator location value effects (value, Address location value)
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
        -> (Address location value -> Evaluator location value effects value)
        -> Evaluator location value effects value
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- localEnv id (body addr)
  v <$ modifyEnv (insert name addr)


-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Addressable location effects
            , Members '[ Reader (Environment location value)
                       , Resumable (AddressError location value)
                       , Resumable (EnvironmentError value)
                       , State (Environment location value)
                       , State (Heap location (Cell location) value)
                       ] effects
            )
         => Name
         -> Evaluator location value effects value
variable name = lookupEnv name >>= maybe (freeVariableError name) deref
