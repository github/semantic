{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
-- * Garbage collection
, gc
, reachable
-- * Effects
, Store(..)
, runStore
, AddressError(..)
, runAddressError
, runAddressErrorWith
) where

import Control.Abstract.Addressable
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Roots
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Abstract.Name
import Data.Semigroup.Reducer
import Prologue

-- | Retrieve the heap.
getHeap :: Member (State (Heap address (Cell address) value)) effects => Evaluator address value effects (Heap address (Cell address) value)
getHeap = get

-- | Set the heap.
putHeap :: Member (State (Heap address (Cell address) value)) effects => Heap address (Cell address) value -> Evaluator address value effects ()
putHeap = put

-- | Update the heap.
modifyHeap :: Member (State (Heap address (Cell address) value)) effects => (Heap address (Cell address) value -> Heap address (Cell address) value) -> Evaluator address value effects ()
modifyHeap = modify'


alloc :: forall address value effects . Member (Store address value) effects => Name -> Evaluator address value effects address
alloc = send . Alloc @address @value

-- | Dereference the given address in the heap, or fail if the address is uninitialized.
deref :: Member (Store address value) effects => address -> Evaluator address value effects value
deref = send . Deref


-- | Write a value to the given address in the 'Store'.
assign :: Member (Store address value) effects
       => address
       -> value
       -> Evaluator address value effects ()
assign address = send . Assign address


-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( Member (Store address value) effects
                 , Member (Env address) effects
                 )
              => Name
              -> Evaluator address value effects address
lookupOrAlloc name = lookupEnv name >>= maybeM (alloc name)


letrec :: ( Member (Store address value) effects
          , Member (Env address) effects
          )
       => Name
       -> Evaluator address value effects value
       -> Evaluator address value effects (value, address)
letrec name body = do
  addr <- lookupOrAlloc name
  v <- locally (bind name addr *> body)
  assign addr v
  pure (v, addr)

-- Lookup/alloc a name passing the address to a body evaluated in a new local environment.
letrec' :: ( Member (Store address value) effects
           , Member (Env address) effects
           )
        => Name
        -> (address -> Evaluator address value effects value)
        -> Evaluator address value effects value
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- locally (body addr)
  v <$ bind name addr


-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Member (Store address value) effects
            , Member (Env address) effects
            , Member (Resumable (EnvironmentError address)) effects
            )
         => Name
         -> Evaluator address value effects value
variable name = lookupEnv name >>= maybeM (freeVariableError name) >>= deref


-- Garbage collection

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Ord address
      , Foldable (Cell address)
      , ValueRoots address value
      )
   => Live address                      -- ^ The set of addresses to consider rooted.
   -> Heap address (Cell address) value -- ^ A heap to collect unreachable addresses within.
   -> Heap address (Cell address) value -- ^ A garbage-collected heap.
gc roots heap = heapRestrict heap (reachable roots heap)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord address
             , Foldable (Cell address)
             , ValueRoots address value
             )
          => Live address                      -- ^ The set of root addresses.
          -> Heap address (Cell address) value -- ^ The heap to trace addresses through.
          -> Live address                      -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) $ case heapLookupAll a heap of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen


-- Effects

data Store address value return where
  Alloc  :: Name             -> Store address value address
  Deref  :: address          -> Store address value value
  Assign :: address -> value -> Store address value ()

runStore :: ( Addressable address effects
            , Member (Resumable (AddressError address value)) effects
            , Member (State (Heap address (Cell address) value)) effects
            , Reducer value (Cell address value)
            )
         => Evaluator address value (Store address value ': effects) a
         -> Evaluator address value effects a
runStore = interpret $ \ eff -> case eff of
  Alloc name -> allocCell name
  Deref addr -> heapLookup addr <$> get >>= maybeM (throwResumable (UnallocatedAddress addr)) >>= derefCell addr >>= maybeM (throwResumable (UninitializedAddress addr))
  Assign addr value -> modifyHeap (heapInsert addr value)


data AddressError address value resume where
  UnallocatedAddress   :: address -> AddressError address value (Cell address value)
  UninitializedAddress :: address -> AddressError address value value

deriving instance Eq address => Eq (AddressError address value resume)
deriving instance Show address => Show (AddressError address value resume)
instance Show address => Show1 (AddressError address value) where
  liftShowsPrec _ _ = showsPrec
instance Eq address => Eq1 (AddressError address value) where
  liftEq _ (UninitializedAddress a) (UninitializedAddress b) = a == b
  liftEq _ (UnallocatedAddress a)   (UnallocatedAddress b)   = a == b
  liftEq _ _                        _                        = False


runAddressError :: Effectful (m address value) => m address value (Resumable (AddressError address value) ': effects) a -> m address value effects (Either (SomeExc (AddressError address value)) a)
runAddressError = runResumable

runAddressErrorWith :: Effectful (m address value) => (forall resume . AddressError address value resume -> m address value effects resume) -> m address value (Resumable (AddressError address value) ': effects) a -> m address value effects a
runAddressErrorWith = runResumableWith
