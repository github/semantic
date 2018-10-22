{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Heap
( Heap
, Live
, getHeap
, putHeap
, box
, alloc
, dealloc
, deref
, assign
-- * Garbage collection
, gc
-- * Effects
, Allocator(..)
, runAllocator
, AllocatorC(..)
, Deref(..)
, runDeref
, DerefC(..)
, AddressError(..)
, runAddressError
, runAddressErrorWith
) where

import Control.Abstract.Evaluator
import Control.Abstract.Roots
import Control.Effect.Carrier
import Data.Abstract.BaseError
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Name
import Data.Span (Span)
import Prologue

-- | Retrieve the heap.
getHeap :: (Member (State (Heap address value)) sig, Carrier sig m) => Evaluator term address value m (Heap address value)
getHeap = get

-- | Set the heap.
putHeap :: (Member (State (Heap address value)) sig, Carrier sig m) => Heap address value -> Evaluator term address value m ()
putHeap = put

-- | Update the heap.
modifyHeap :: (Member (State (Heap address value)) sig, Carrier sig m) => (Heap address value -> Heap address value) -> Evaluator term address value m ()
modifyHeap = modify

box :: ( Member (Allocator address) sig
       , Member (Deref value) sig
       , Member Fresh sig
       , Member (State (Heap address value)) sig
       , Ord address
       , Carrier sig m
       )
    => value
    -> Evaluator term address value m address
box val = do
  name <- gensym
  addr <- alloc name
  assign addr val
  pure addr

alloc :: (Member (Allocator address) sig, Carrier sig m) => Name -> Evaluator term address value m address
alloc = send . flip Alloc ret

dealloc :: (Member (State (Heap address value)) sig, Ord address, Carrier sig m) => address -> Evaluator term address value m ()
dealloc addr = modifyHeap (heapDelete addr)

-- | Dereference the given address in the heap, or fail if the address is uninitialized.
deref :: ( Member (Deref value) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (AddressError address value))) sig
         , Member (State (Heap address value)) sig
         , Ord address
         , Carrier sig m
         )
      => address
      -> Evaluator term address value m value
deref addr = gets (heapLookup addr) >>= maybeM (throwAddressError (UnallocatedAddress addr)) >>= send . flip DerefCell ret >>= maybeM (throwAddressError (UninitializedAddress addr))


-- | Write a value to the given address in the 'Allocator'.
assign :: ( Member (Deref value) sig
          , Member (State (Heap address value)) sig
          , Ord address
          , Carrier sig m
          )
       => address
       -> value
       -> Evaluator term address value m ()
assign addr value = do
  heap <- getHeap
  cell <- send (AssignCell value (fromMaybe lowerBound (heapLookup addr heap)) ret)
  putHeap (heapInit addr cell heap)


-- Garbage collection

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Member (State (Heap address value)) sig
      , Ord address
      , ValueRoots address value
      , Carrier sig m
      )
   => Live address                       -- ^ The set of addresses to consider rooted.
   -> Evaluator term address value m ()
gc roots = modifyHeap (heapRestrict <*> reachable roots)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord address
             , ValueRoots address value
             )
          => Live address       -- ^ The set of root addresses.
          -> Heap address value -- ^ The heap to trace addresses through.
          -> Live address       -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) $ case heapLookupAll a heap of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen


-- Effects

data Allocator address (m :: * -> *) k
  = Alloc Name (address -> k)
  deriving (Functor)

instance HFunctor (Allocator address) where
  hmap _ (Alloc name k) = Alloc name k

instance Effect (Allocator address) where
  handle state handler (Alloc name k) = Alloc name (handler . (<$ state) . k)

runAllocator :: Carrier (Allocator address :+: sig) (AllocatorC address (Eff m))
             => Evaluator term address value (AllocatorC address (Eff m)) a
             -> Evaluator term address value m a
runAllocator = Evaluator . runAllocatorC . interpret . runEvaluator

newtype AllocatorC address m a = AllocatorC { runAllocatorC :: m a }


data Deref value (m :: * -> *) k
  = DerefCell        (Set value) (Maybe value -> k)
  | AssignCell value (Set value) (Set value   -> k)
  deriving (Functor)

instance HFunctor (Deref value) where
  hmap _ (DerefCell        cell k) = DerefCell        cell k
  hmap _ (AssignCell value cell k) = AssignCell value cell k

instance Effect (Deref value) where
  handle state handler (DerefCell        cell k) = DerefCell        cell (handler . (<$ state) . k)
  handle state handler (AssignCell value cell k) = AssignCell value cell (handler . (<$ state) . k)

runDeref :: Carrier (Deref value :+: sig) (DerefC (Evaluator term address value m))
         => Evaluator term address value (DerefC (Evaluator term address value m)) a
         -> Evaluator term address value m a
runDeref = runDerefC . interpret . runEvaluator

newtype DerefC m a = DerefC { runDerefC :: m a }



data AddressError address value resume where
  UnallocatedAddress   :: address -> AddressError address value (Set value)
  UninitializedAddress :: address -> AddressError address value value

instance (NFData address) => NFData1 (AddressError address value) where
  liftRnf _ x = case x of
    UnallocatedAddress a -> rnf a
    UninitializedAddress a -> rnf a

instance (NFData address, NFData resume) => NFData (AddressError address value resume) where
  rnf = liftRnf rnf

deriving instance Eq address => Eq (AddressError address value resume)
deriving instance Show address => Show (AddressError address value resume)
instance Show address => Show1 (AddressError address value) where
  liftShowsPrec _ _ = showsPrec
instance Eq address => Eq1 (AddressError address value) where
  liftEq _ (UninitializedAddress a) (UninitializedAddress b) = a == b
  liftEq _ (UnallocatedAddress a)   (UnallocatedAddress b)   = a == b
  liftEq _ _                        _                        = False

throwAddressError :: ( Member (Resumable (BaseError (AddressError address body))) sig
                     , Member (Reader ModuleInfo) sig
                     , Member (Reader Span) sig
                     , Carrier sig m
                     )
                  => AddressError address body resume
                  -> Evaluator term address value m resume
throwAddressError = throwBaseError

runAddressError :: (Carrier sig m, Effect sig)
                => Evaluator term address value (ResumableC (BaseError (AddressError address value)) (Evaluator term address value m)) a
                -> Evaluator term address value m (Either (SomeError (BaseError (AddressError address value))) a)
runAddressError = runResumable . runEvaluator

runAddressErrorWith :: Carrier sig m
                    => (forall resume . (BaseError (AddressError address value)) resume -> Evaluator term address value m resume)
                    -> Evaluator term address value (ResumableWithC (BaseError (AddressError address value)) (Evaluator term address value m)) a
                    -> Evaluator term address value m a
runAddressErrorWith f = runResumableWith f . runEvaluator
