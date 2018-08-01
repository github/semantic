{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}
module Control.Abstract.Heap
( Heap
, Configuration(..)
, Live
, getConfiguration
, getHeap
, putHeap
, box
, alloc
, deref
, assign
, lookupOrAlloc
, letrec
, letrec'
, variable
-- * Garbage collection
, gc
-- * Effects
, Allocator(..)
, runAllocator
, Deref(..)
, runDeref
, AddressError(..)
, runAddressError
, runAddressErrorWith
) where

import Control.Abstract.Addressable
import Control.Abstract.Context (currentErrorContext)
import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Roots
import Control.Abstract.TermEvaluator
import Data.Abstract.Configuration
import Data.Abstract.ErrorContext
import Data.Abstract.Heap
import Data.Abstract.Live
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Name
import Data.Semigroup.Reducer
import Data.Span (Span)
import Prologue

-- | Get the current 'Configuration' with a passed-in term.
getConfiguration :: (Member (Reader (Live address)) effects, Member (Env address) effects, Member (State (Heap address (Cell address) value)) effects) => term -> TermEvaluator term address value effects (Configuration term address (Cell address) value)
getConfiguration term = Configuration term <$> TermEvaluator askRoots <*> TermEvaluator getEnv <*> TermEvaluator getHeap


-- | Retrieve the heap.
getHeap :: Member (State (Heap address (Cell address) value)) effects => Evaluator address value effects (Heap address (Cell address) value)
getHeap = get

-- | Set the heap.
putHeap :: Member (State (Heap address (Cell address) value)) effects => Heap address (Cell address) value -> Evaluator address value effects ()
putHeap = put

-- | Update the heap.
modifyHeap :: Member (State (Heap address (Cell address) value)) effects => (Heap address (Cell address) value -> Heap address (Cell address) value) -> Evaluator address value effects ()
modifyHeap = modify'

box :: ( Member (Allocator address value) effects
       , Member Fresh effects
       )
    => value
    -> Evaluator address value effects address
box val = do
  name <- gensym
  addr <- alloc name
  assign addr val
  pure addr

alloc :: Member (Allocator address value) effects => Name -> Evaluator address value effects address
alloc = sendAllocator . Alloc

-- | Dereference the given address in the heap, or fail if the address is uninitialized.
deref :: Member (Deref address value) effects => address -> Evaluator address value effects value
deref = send . Deref


-- | Write a value to the given address in the 'Allocator'.
assign :: Member (Allocator address value) effects
       => address
       -> value
       -> Evaluator address value effects ()
assign address = send . Assign address


-- | Look up or allocate an address for a 'Name'.
lookupOrAlloc :: ( Member (Allocator address value) effects
                 , Member (Env address) effects
                 )
              => Name
              -> Evaluator address value effects address
lookupOrAlloc name = lookupEnv name >>= maybeM (alloc name)


letrec :: ( Member (Allocator address value) effects
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
letrec' :: ( Member (Allocator address value) effects
           , Member (Env address) effects
           )
        => Name
        -> (address -> Evaluator address value effects a)
        -> Evaluator address value effects a
letrec' name body = do
  addr <- lookupOrAlloc name
  v <- locally (body addr)
  v <$ bind name addr


-- | Look up and dereference the given 'Name', throwing an exception for free variables.
variable :: ( Member (Env address) effects
            , Member (Resumable (EnvironmentError address)) effects
            )
         => Name
         -> Evaluator address value effects address
variable name = lookupEnv name >>= maybeM (freeVariableError name)


-- Garbage collection

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: Member (Allocator address value) effects
   => Live address                       -- ^ The set of addresses to consider rooted.
   -> Evaluator address value effects ()
gc roots = sendAllocator (GC roots)

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

sendAllocator :: Member (Allocator address value) effects => Allocator address value (Eff effects) return -> Evaluator address value effects return
sendAllocator = send

data Allocator address value (m :: * -> *) return where
  Alloc  :: Name             -> Allocator address value m address
  Assign :: address -> value -> Allocator address value m ()
  GC     :: Live address     -> Allocator address value m ()

data Deref address value (m :: * -> *) return where
  Deref  :: address          -> Deref address value m value

runAllocator :: ( Allocatable address effects
                , Foldable (Cell address)
                , Member (State (Heap address (Cell address) value)) effects
                , PureEffects effects
                , Reducer value (Cell address value)
                , ValueRoots address value
                )
             => Evaluator address value (Allocator address value ': effects) a
             -> Evaluator address value effects a
runAllocator = interpret $ \ eff -> case eff of
  Alloc name -> allocCell name
  Assign addr value -> modifyHeap (heapInsert addr value)
  GC roots -> modifyHeap (heapRestrict <*> reachable roots)

runDeref :: ( Derefable address effects
            , PureEffects effects
            , Member (Reader ModuleInfo) effects
            , Member (Reader Span) effects
            , Member (Resumable (BaseError (AddressError address value))) effects
            , Member (State (Heap address (Cell address) value)) effects
            )
         => Evaluator address value (Deref address value ': effects) a
         -> Evaluator address value effects a
runDeref = interpret $ \ eff -> case eff of
  Deref addr -> heapLookup addr <$> get >>= maybeM (throwAddressError (UnallocatedAddress addr)) >>= derefCell addr >>= maybeM (throwAddressError (UninitializedAddress addr))

instance PureEffect (Allocator address value)

instance Effect (Allocator address value) where
  handleState c dist (Request (Alloc name) k) = Request (Alloc name) (dist . (<$ c) . k)
  handleState c dist (Request (Assign addr value) k) = Request (Assign addr value) (dist . (<$ c) . k)
  handleState c dist (Request (GC roots) k) = Request (GC roots) (dist . (<$ c) . k)

instance PureEffect (Deref address value)

instance Effect (Deref address value) where
  handleState c dist (Request (Deref addr) k) = Request (Deref addr) (dist . (<$ c) . k)

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

throwAddressError :: ( Member (Resumable (BaseError (AddressError address body))) effects
                     , Member (Reader ModuleInfo) effects
                     , Member (Reader Span) effects
                     )
                  => AddressError address body resume
                  -> Evaluator address value effects resume
throwAddressError err = currentErrorContext >>= \ errorContext -> throwResumable $ BaseError errorContext err

runAddressError :: ( Effectful (m address value)
                   , Effects effects
                   )
                => m address value (Resumable (BaseError (AddressError address value)) ': effects) a
                -> m address value effects (Either (SomeExc (BaseError (AddressError address value))) a)
runAddressError = runResumable

runAddressErrorWith :: (Effectful (m address value), Effects effects)
                    => (forall resume . (BaseError (AddressError address value)) resume -> m address value effects resume)
                    -> m address value (Resumable (BaseError (AddressError address value)) ': effects) a
                    -> m address value effects a
runAddressErrorWith = runResumableWith
