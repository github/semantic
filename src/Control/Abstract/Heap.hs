{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Heap
( Heap
, getHeap
, putHeap
, modifyHeap
, alloc
, deref
, assign
, lookupOrAlloc
, letrec
, letrec'
, variable
-- * Effects
, Allocator(..)
, runAllocator
, AddressError(..)
, runAddressError
, runAddressErrorWith
) where

import Control.Abstract.Addressable
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.Heap
import Data.Abstract.Name
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


alloc :: Member (Allocator location value) effects => Name -> Evaluator location value effects (Address location value)
alloc = send . Alloc

-- | Dereference the given 'Address'in the heap, or fail if the address is uninitialized.
deref :: Member (Allocator location value) effects => Address location value -> Evaluator location value effects value
deref = send . Deref


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
lookupOrAlloc :: Members '[ Allocator location value
                          , Reader (Environment location value)
                          , State (Environment location value)
                          ] effects
              => Name
              -> Evaluator location value effects (Address location value)
lookupOrAlloc name = lookupEnv name >>= maybe (alloc name) pure


letrec :: ( Members '[ Allocator location value
                     , Reader (Environment location value)
                     , State (Environment location value)
                     , State (Heap location (Cell location) value)
                     ] effects
          , Ord location
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
letrec' :: Members '[ Allocator location value
                    , Reader (Environment location value)
                    , State (Environment location value)
                    ] effects
        => Name
        -> (Address location value -> Evaluator location value effects value)
        -> Evaluator location value effects value
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- localEnv id (body addr)
  v <$ modifyEnv (insert name addr)


-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: Members '[ Allocator location value
                     , Reader (Environment location value)
                     , Resumable (EnvironmentError value)
                     , State (Environment location value)
                     , State (Heap location (Cell location) value)
                     ] effects
         => Name
         -> Evaluator location value effects value
variable name = lookupEnv name >>= maybe (freeVariableError name) deref


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
