{-# LANGUAGE GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}
module Control.Abstract.Heap
( Heap
, HeapError
, Address(..)
, Position(..)
, Configuration(..)
, Live
, getHeap
, putHeap
, alloc
, deref
, assign
, newFrame
, currentFrame
, withScopeAndFrame
, withChildFrame
, define
, withFrame
-- * Garbage collection
, gc
-- * Effects
, Deref(..)
, AddressError(..)
, runAddressError
, runAddressErrorWith
) where

import Control.Abstract.Context (withCurrentCallStack)
import Control.Abstract.Evaluator
import Control.Abstract.Roots
import Data.Abstract.Configuration
import Data.Abstract.BaseError
import qualified Data.Abstract.Heap as Heap
import Data.Abstract.Heap (Heap, Position(..))
import Control.Abstract.ScopeGraph
import Data.Abstract.Live
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Name
import Data.Span (Span)
import qualified Data.Set as Set
import Prologue

-- | Evaluates an action locally the scope and frame of the given frame address.
withScopeAndFrame :: forall address value effects m a. (
                      Effectful (m address value)
                    , Member (Reader ModuleInfo) effects
                    , Member (Reader Span) effects
                    , Member (Resumable (BaseError (HeapError address))) effects
                    , Member (Resumable (BaseError (ScopeError address))) effects
                    , Member (State (Heap address address value)) effects
                    , Member (State (ScopeGraph address)) effects
                    , Ord address
                    )
                  => address
                  -> m address value effects a
                  -> m address value effects a
withScopeAndFrame address action = raiseEff $ do
  scope <- lowerEff (scopeLookup @address @value address)
  lowerEff $ withScope scope (withFrame address action)

scopeLookup :: forall address value effects. (Ord address, Member (Reader ModuleInfo) effects, Member (Reader Span) effects, Member (Resumable (BaseError (HeapError address))) effects, Member (State (Heap address address value)) effects, Member (State (ScopeGraph address)) effects) => address -> Evaluator address value effects address
scopeLookup address = maybeM (throwHeapError (LookupError address)) =<< Heap.scopeLookup address <$> get @(Heap address address value)

-- | Retrieve the heap.
getHeap :: Member (State (Heap address address value)) effects => Evaluator address value effects (Heap address address value)
getHeap = get

-- | Set the heap.
putHeap :: Member (State (Heap address address value)) effects => Heap address address value -> Evaluator address value effects ()
putHeap = put

-- | Update the heap.
modifyHeap :: Member (State (Heap address address value)) effects => (Heap address address value -> Heap address address value) -> Evaluator address value effects ()
modifyHeap = modify'

currentFrame :: forall address value effects. ( Member (State (Heap address address value)) effects
                , Member (Reader ModuleInfo) effects
                , Member (Reader Span) effects
                , Member (Resumable (BaseError (HeapError address))) effects
                )
             => Evaluator address value effects address
currentFrame = maybeM (throwHeapError EmptyHeapError) =<< (Heap.currentFrame <$> get @(Heap address address value))

-- | Inserts a new frame into the heap with the given scope and links.
newFrame :: forall address value effects. (
            Member (State (Heap address address value)) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          , Member (Resumable (BaseError (HeapError address))) effects
          , Ord address
          , Member (Allocator address) effects
          , Member (State (ScopeGraph address)) effects
          , Member Fresh effects
          )
         => address
         -> Map EdgeLabel (Map address address)
         -> Evaluator address value effects address
newFrame scope links = do
  name <- gensym
  address <- alloc name
  modify @(Heap address address value) (Heap.newFrame scope address links)
  pure address

-- | Evaluates the action within the frame of the given frame address.
withFrame :: forall address effects m value a. (
             Member (Resumable (BaseError (HeapError address))) effects
           , Member (Reader ModuleInfo) effects
           , Member (Reader Span) effects
           , Effectful (m address value)
           , Member (State (Heap address address value)) effects)
          => address
          -> m address value effects a
          -> m address value effects a
withFrame address action = raiseEff $ do
    prevFrame <- (lowerEff (currentFrame @address @value))
    modify @(Heap address address value) (\h -> h { Heap.currentFrame = Just address })
    value <- (lowerEff action)
    modify @(Heap address address value) (\h -> h { Heap.currentFrame = Just prevFrame })
    pure value

-- box :: ( Member (Allocator address) effects
--        , Member (Deref value) effects
--        , Member Fresh effects
--        , Member (State (Heap address address value)) effects
--        , Ord address
--        )
--     => value
--     -> Evaluator address value effects address
-- box val = do
--   name <- gensym
--   addr <- alloc name
--   assign addr (Declaration name) val -- TODO This is probably wrong
--   pure addr

-- | Define a declaration and assign the value of an action in the current frame.
define :: ( HasCallStack
          , Member (Allocator (Address address)) effects
          , Member (Deref value) effects
          , Member (Reader ModuleInfo) effects
          , Member (Reader Span) effects
          , Member (State (Heap address address value)) effects
          , Member (State (ScopeGraph address)) effects
          , Member (Resumable (BaseError (ScopeError address))) effects
          , Ord address
          )
       => Declaration
       -> Evaluator address value effects value
       -> Evaluator address value effects value
define declaration def = withCurrentCallStack callStack $ do
  span <- ask @Span -- TODO: This Span is most definitely wrong
  addr <- declare declaration span Nothing
  value <- def
  value <$ assign addr value

-- | Associate an empty child scope with a declaration and then locally evaluate the body within an associated frame.
withChildFrame :: ( Member (Allocator address) effects
                  , Member (State (Heap address address value)) effects
                  , Member (State (ScopeGraph address)) effects
                  , Member Fresh effects
                  , Member (Reader ModuleInfo) effects
                  , Member (Reader Span) effects
                  , Member (Resumable (BaseError (HeapError address))) effects
                  , Member (Resumable (BaseError (ScopeError address))) effects
                  , Member (Deref value) effects
                  , Ord address
                  )
                => Declaration
                -> (address -> Evaluator address value effects a)
                -> Evaluator address value effects a
withChildFrame declaration body = do
  scope <- newScope mempty
  putDeclarationScope declaration scope
  frame <- newFrame scope mempty
  withScopeAndFrame frame (body frame)

-- | Dereference the given address in the heap, or fail if the address is uninitialized.
deref :: ( Member (Deref value) effects
         , Member (Reader ModuleInfo) effects
         , Member (Reader Span) effects
         , Member (Resumable (BaseError (AddressError address value))) effects
         , Member (State (Heap address address value)) effects
         , Ord address
         )
      => Address address
      -> Evaluator address value effects value
-- TODO: THIS IS WRONG we need to call Heap.lookup
deref addr@Address{..} = gets (Heap.getSlot addr) >>= maybeM (throwAddressError (UnallocatedAddress address)) >>= send . DerefCell >>= maybeM (throwAddressError (UninitializedAddress address))


-- | Write a value to the given address in the 'Heap'.
assign :: ( Member (Deref value) effects
          , Member (State (Heap address address value)) effects
          , Ord address
          )
       => Address address
       -> value
       -> Evaluator address value effects ()
assign addr value = do
  heap <- getHeap
  cell <- send (AssignCell value (fromMaybe lowerBound (Heap.getSlot addr heap)))
  putHeap (Heap.setSlot addr cell heap)


-- Garbage collection

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Member (State (Heap address address value)) effects
      , Ord address
      , ValueRoots address value
      )
   => Live address                       -- ^ The set of addresses to consider rooted.
   -> Evaluator address value effects ()
gc roots =
  -- TODO: Implement frame garbage collection
  undefined
  -- modifyHeap (heapRestrict <*> reachable roots)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord address
             , ValueRoots address value
             )
          => Live address       -- ^ The set of root addresses.
          -> Heap address address value -- ^ The heap to trace addresses through.
          -> Live address       -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> undefined -- go (liveInsert a seen) $ case heapLookupAll a heap of
            -- Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            -- _           -> seen


-- Effects
data Deref value (m :: * -> *) return where
  DerefCell  :: Set value          -> Deref value m (Maybe value)
  AssignCell :: value -> Set value -> Deref value m (Set value)

instance PureEffect (Deref value)

instance Effect (Deref value) where
  handleState c dist (Request (DerefCell cell) k) = Request (DerefCell cell) (dist . (<$ c) . k)
  handleState c dist (Request (AssignCell value cell) k) = Request (AssignCell  value cell) (dist . (<$ c) . k)

data HeapError address resume where
  EmptyHeapError :: HeapError address address
  LookupError :: address -> HeapError address address

deriving instance Eq address => Eq (HeapError address resume)
deriving instance Show address => Show (HeapError address resume)
instance Show address => Show1 (HeapError address) where
  liftShowsPrec _ _ = showsPrec
instance Eq address => Eq1 (HeapError address) where
  liftEq _ EmptyHeapError EmptyHeapError = True

throwHeapError  :: ( Member (Resumable (BaseError (HeapError address))) effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   )
                => HeapError address resume
                -> Evaluator address value effects resume
throwHeapError = throwBaseError

runHeapError :: ( Effectful (m address value)
                   , Effects effects
                   )
                => m address value (Resumable (BaseError (HeapError address)) ': effects) a
                -> m address value effects (Either (SomeExc (BaseError (HeapError address))) a)
runHeapError = runResumable

data AddressError address value resume where
  UnallocatedAddress   :: address -> AddressError address value (Set value)
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
throwAddressError = throwBaseError

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
