{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Abstract.Heap
( Heap
, HeapError(..)
, Slot(..)
, Position(..)
, Live
, getHeap
, putHeap
, putSlotDeclarationScope
, alloc
, dealloc
, maybeLookupDeclaration
, lookupSlot
, lookupDeclarationFrame
, deref
, assign
, newFrame
, CurrentFrame(..)
, currentFrame
, lookupFrame
, withScopeAndFrame
, withLexicalScopeAndFrame
, withChildFrame
, define
, withFrame
-- * Garbage collection
, gc
-- * Effects
, Allocator(..)
, Deref(..)
, runDeref
, DerefC(..)
, AddressError(..)
, runHeapError
, runAddressError
, runAddressErrorWith
, throwAddressError
, runHeapErrorWith
, throwHeapError
, insertFrameLink
, scopeLookup
) where

import           Analysis.Name
import           Control.Abstract.Context (withCurrentCallStack)
import           Control.Abstract.Evaluator
import           Control.Abstract.Roots
import           Control.Abstract.ScopeGraph hiding (ScopeError (..))
import           Control.Abstract.ScopeGraph (ScopeError)
import           Control.Algebra
import           Control.Carrier.Resumable.Either (SomeError (..))
import qualified Control.Carrier.Resumable.Either as Either
import qualified Control.Carrier.Resumable.Resume as With
import           Data.Abstract.BaseError
import           Data.Abstract.Heap (Heap, Position (..))
import qualified Data.Abstract.Heap as Heap
import           Data.Abstract.Live
import           Data.Abstract.Module (ModuleInfo)
import           Data.Abstract.ScopeGraph (Kind (..), Path (..), putDeclarationScopeAtPosition)
import           Data.Functor.Classes
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Exts
import           Data.Semilattice.Lower
import           Data.Set (Set)
import           GHC.Generics (Generic1)
import           GHC.Stack
import           Source.Span (Span)


-- | Evaluates an action locally the scope and frame of the given frame address.
withScopeAndFrame :: ( Ord address
                     , Has (Reader ModuleInfo) sig m
                     , Has (Reader Span) sig m
                     , Has (Resumable (BaseError (HeapError address))) sig m
                     , Has (Reader (CurrentFrame address)) sig m
                     , Has (Reader (CurrentScope address)) sig m
                     , Has (State (Heap address address value)) sig m
                     )
                  => address
                  -> Evaluator term address value m a
                  -> Evaluator term address value m a
withScopeAndFrame address action = do
  scope <- scopeLookup address
  withScope scope (withFrame address action)

-- | Evaluates an action locally the scope and frame of the given frame address.
withLexicalScopeAndFrame :: ( Ord address
                            , Has (Reader ModuleInfo) sig m
                            , Has (Reader Span) sig m
                            , Has (Resumable (BaseError (HeapError address))) sig m
                            , Has (State (Heap address address value)) sig m
                            , Has (State (ScopeGraph address)) sig m
                            , Has (Reader (CurrentFrame address)) sig m
                            , Has (Reader (CurrentScope address)) sig m
                            , Has (Allocator address) sig m
                            , Has Fresh sig m
                            )
                         => Evaluator term address value m a
                         -> Evaluator term address value m a
withLexicalScopeAndFrame action = do
  currentScope' <- currentScope
  currentFrame' <- currentFrame
  let (scopeEdges, frameEdges) = (Map.singleton Lexical [ currentScope' ], Map.singleton Lexical (Map.singleton currentScope' currentFrame'))
  scope <- newScope scopeEdges
  frame <- newFrame scope frameEdges
  withScopeAndFrame frame action

-- | Lookup a scope address for a given frame address.
scopeLookup :: ( Ord address
               , Has (Reader ModuleInfo) sig m
               , Has (Reader Span) sig m
               , Has (Resumable (BaseError (HeapError address))) sig m
               , Has (State (Heap address address value)) sig m

               )
            => address
            -> Evaluator term address value m address
scopeLookup address = maybeM (throwHeapError (LookupAddressError address)) =<< Heap.scopeLookup address <$> getHeap

getHeap :: Has (State (Heap address address value)) sig m => Evaluator term address value m (Heap address address value)
getHeap = get

-- | Set the heap.
putHeap :: Has (State (Heap address address value)) sig m => Heap address address value -> Evaluator term address value m ()
putHeap = put

-- | Update the heap.
modifyHeap :: Has (State (Heap address address value)) sig m => (Heap address address value -> Heap address address value) -> Evaluator term address value m ()
modifyHeap = modify

newtype CurrentFrame address = CurrentFrame { unCurrentFrame :: address }

-- | Retrieve the heap.
currentFrame :: Has (Reader (CurrentFrame address)) sig m
             => Evaluator term address value m address
currentFrame = asks unCurrentFrame


-- | Inserts a new frame into the heap with the given scope and links.
newFrame :: ( Has (Allocator address) sig m
            , Has Fresh sig m
            , Has (State (Heap address address value)) sig m
            , Ord address
            )
         => address
         -> Map EdgeLabel (Map address address)
         -> Evaluator term address value m address
newFrame scope links = do
  name <- gensym
  address <- alloc name
  modifyHeap (Heap.newFrame scope address links)
  pure address

-- | Evaluates the action within the frame of the given frame address.
withFrame :: Has (Reader (CurrentFrame address)) sig m
          => address
          -> Evaluator term address value m a -- Not sure about this `sig` here (substituting `sig` for `effects`)
          -> Evaluator term address value m a
withFrame address = local (const (CurrentFrame address))

-- | Define a declaration and assign the value of an action in the current frame.
define :: ( HasCallStack
          , Has (Deref value) sig m
          , Has (Reader ModuleInfo) sig m
          , Has (Reader Span) sig m
          , Has (Reader (CurrentFrame address)) sig m
          , Has (Reader (CurrentScope address)) sig m
          , Has (State (Heap address address value)) sig m
          , Has (State (ScopeGraph address)) sig m
          , Has (Resumable (BaseError (ScopeError address))) sig m
          , Has (Resumable (BaseError (HeapError address))) sig m
          , Ord address
          )
       => Declaration
       -> Relation
       -> AccessControl
       -> Evaluator term address value m value
       -> Evaluator term address value m ()
define declaration rel accessControl def = withCurrentCallStack callStack $ do
  -- TODO: This span is still wrong.
  declare declaration rel accessControl lowerBound Unknown Nothing
  slot <- lookupSlot declaration
  value <- def
  assign slot value

-- | Associate an empty child scope with a declaration and then locally evaluate the body within an associated frame.
withChildFrame :: ( Has (Allocator address) sig m
                  , Has (State (Heap address address value)) sig m
                  , Has (State (ScopeGraph address)) sig m
                  , Has (Reader (CurrentFrame address)) sig m
                  , Has (Reader (CurrentScope address)) sig m
                  , Has Fresh sig m
                  , Has (Reader ModuleInfo) sig m
                  , Has (Reader Span) sig m
                  , Has (Resumable (BaseError (HeapError address))) sig m
                  , Ord address
                  )
                => Declaration
                -> (address -> Evaluator term address value m a)
                -> Evaluator term address value m a
withChildFrame declaration body = do
  scope <- newPreludeScope mempty
  putDeclarationScope declaration scope
  frame <- newFrame scope mempty
  withScopeAndFrame frame (body frame)

-- | Dereference the given address in the heap, or fail if the address is uninitialized.
deref :: ( Has (Deref value) sig m
         , Has (Reader ModuleInfo) sig m
         , Has (Reader Span) sig m
         , Has (Resumable (BaseError (AddressError address value))) sig m
         , Has (State (Heap address address value)) sig m
         , Ord address
         )
      => Slot address
      -> Evaluator term address value m value
deref slot@Slot{..} = do
  maybeSlotValue <- gets (Heap.getSlotValue slot)
  slotValue <- maybeM (throwAddressError (UnallocatedSlot slot)) maybeSlotValue
  eff <- send $ DerefCell slotValue pure
  maybeM (throwAddressError $ UninitializedSlot slot) eff

putSlotDeclarationScope :: ( Has (State (Heap address address value)) sig m
                           , Has (State (ScopeGraph address)) sig m
                           , Has (Resumable (BaseError (HeapError address))) sig m
                           , Has (Reader ModuleInfo) sig m
                           , Has (Reader Span) sig m
                           , Ord address
                           )
                        => Slot address
                        -> Maybe address
                        -> Evaluator term address value m ()
putSlotDeclarationScope Slot{..} assocScope = do
  scopeAddress <- scopeLookup frameAddress
  modify (putDeclarationScopeAtPosition scopeAddress position assocScope)


maybeLookupDeclaration :: ( Has (Reader (CurrentFrame address)) sig m
                          , Has (Reader (CurrentScope address)) sig m
                          , Has (Reader ModuleInfo) sig m
                          , Has (Reader Span) sig m
                          , Has (Resumable (BaseError (HeapError address))) sig m
                          , Has (State (Heap address address value)) sig m
                          , Has (State (ScopeGraph address)) sig m
                          , Ord address
                          )
                       => Declaration
                       -> Evaluator term address value m (Maybe (Slot address))
maybeLookupDeclaration decl = do
  path <- maybeLookupScopePath decl
  case path of
    Just path -> do
      frameAddress <- lookupFrameAddress path
      pure (Just (Slot frameAddress (Heap.pathPosition path)))
    Nothing -> pure Nothing

lookupSlot :: ( Has (Reader (CurrentFrame address)) sig m
              , Has (Reader (CurrentScope address)) sig m
              , Has (Reader ModuleInfo) sig m
              , Has (Reader Span) sig m
              , Has (Resumable (BaseError (HeapError address))) sig m
              , Has (Resumable (BaseError (ScopeError address))) sig m
              , Has (State (Heap address address value)) sig m
              , Has (State (ScopeGraph address)) sig m
              , Ord address
              )
           => Declaration
           -> Evaluator term address value m (Slot address)
lookupSlot decl = do
  path <- lookupScopePath decl
  frameAddress <- lookupFrameAddress path
  pure (Slot frameAddress (Heap.pathPosition path))

lookupDeclarationFrame :: ( Has (State (Heap address address value)) sig m
                          , Has (State (ScopeGraph address)) sig m
                          , Has (Reader (CurrentFrame address)) sig m
                          , Has (Reader (CurrentScope address)) sig m
                          , Has (Reader ModuleInfo) sig m
                          , Has (Reader Span) sig m
                          , Has (Resumable (BaseError (ScopeError address))) sig m
                          , Has (Resumable (BaseError (HeapError address))) sig m
                          , Ord address
                          )
                       => Declaration
                       -> Evaluator term address value m address
lookupDeclarationFrame decl = do
  path <- lookupScopePath decl
  lookupFrameAddress path

lookupFrame :: ( Has (State (Heap address address value)) sig m
               , Has (Reader ModuleInfo) sig m
               , Has (Reader Span) sig m
               , Has (Resumable (BaseError (HeapError address))) sig m
               , Ord address
               )
             => address
             -> Evaluator term address value m (Heap.Frame address address value)
lookupFrame address = do
  heap <- getHeap
  maybeM (throwHeapError (LookupFrameError address)) (Heap.frameLookup address heap)

-- | Follow a path through the heap and return the frame address associated with the declaration.
lookupFrameAddress :: ( Has (State (Heap address address value)) sig m
                      , Has (Reader (CurrentFrame address)) sig m
                      , Has (Reader ModuleInfo) sig m
                      , Has (Reader Span) sig m
                      , Has (Resumable (BaseError (HeapError address))) sig m
                      , Ord address
                      )
                   => Path address
                   -> Evaluator term address value m address
lookupFrameAddress path = go path =<< currentFrame
  where
    go path address = case path of
      Hole -> throwHeapError (LookupLinkError path)
      DPath _ _ -> pure address
      p@(EPath edge nextScopeAddress path') -> do
        linkMap <- frameLinks address
        let frameAddress = do
              scopeMap <- Map.lookup edge linkMap
              Map.lookup nextScopeAddress scopeMap
        maybe (throwHeapError $ LookupLinkError p) (go path') frameAddress

frameLinks :: ( Has (Reader ModuleInfo) sig m
              , Has (Reader Span) sig m
              , Has (Resumable (BaseError (HeapError address))) sig m
              , Has (State (Heap address address value)) sig m
              , Ord address
              )
           => address
           -> Evaluator term address value m (Map EdgeLabel (Map address address)) -- TODO: Change this to Map scope address
frameLinks address = maybeM (throwHeapError (LookupLinksError address)) . Heap.frameLinks address =<< getHeap


insertFrameLink :: ( Has (Reader (CurrentFrame address)) sig m
                   , Has (Reader ModuleInfo) sig m
                   , Has (Reader Span) sig m
                   , Has (Resumable (BaseError (HeapError address))) sig m
                   , Has (State (Heap address address value)) sig m
                   , Ord address
                   )
                => EdgeLabel
                -> Map address address
                -> Evaluator term address value m ()
insertFrameLink label linkMap = do
  frameAddress <- currentFrame
  heap <- getHeap
  currentFrame <- maybeM (throwHeapError (LookupFrameError frameAddress)) (Heap.frameLookup frameAddress heap)
  let newCurrentFrame = currentFrame
        { Heap.links = Map.alter (\val -> val <> Just linkMap) label (Heap.links currentFrame) }
  modify (Heap.insertFrame frameAddress newCurrentFrame)


-- | Write a value to the given frame address in the 'Heap'.
assign :: ( Has (Deref value) sig m
          , Has (State (Heap address address value)) sig m
          , Ord address
          )
       => Slot address
       -> value
       -> Evaluator term address value m ()
assign addr value = do
  heap <- getHeap
  cell <- send (AssignCell value (fromMaybe lowerBound (Heap.getSlotValue addr heap)) pure)
  putHeap (Heap.setSlot addr cell heap)

dealloc :: ( Has (State (Heap address address value)) sig m
           , Ord address
           )
        => Slot address
        -> Evaluator term address value m ()
dealloc addr = modifyHeap (Heap.deleteSlot addr)


-- Garbage collection

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
gc :: ( Has (State (Heap address address value)) sig m
      , Ord address
      , Ord value
      , ValueRoots address value
      )
   => Live address                       -- ^ The set of addresses to consider rooted.
   -> Evaluator term address value m ()
gc roots = modifyHeap (Heap.heapRestrict <*> reachable roots)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: ( Ord address
             , Ord value
             , ValueRoots address value
             )
          => Live address       -- ^ The set of root addresses.
          -> Heap address address value -- ^ The heap to trace addresses through.
          -> Live address       -- ^ The set of addresses reachable from the root set.
reachable roots heap = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (a, as) -> go (liveInsert a seen) $ case Heap.heapLookupAll a heap of
            Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            _           -> seen


-- Effects

data Deref value (m :: * -> *) k
  = DerefCell        (Set value) (Maybe value -> m k)
  | AssignCell value (Set value) (Set value   -> m k)
  deriving (Functor, Generic1)

instance HFunctor (Deref value)
instance Effect   (Deref value)

runDeref :: Evaluator term address value (DerefC address value m) a
         -> Evaluator term address value m a
runDeref = raiseHandler runDerefC

newtype DerefC address value m a = DerefC { runDerefC :: m a }
  deriving (Alternative, Applicative, Functor, Monad)



data HeapError address resume where
  CurrentFrameError :: HeapError address address
  LookupAddressError :: address -> HeapError address address
  LookupFrameError :: address -> HeapError address (Heap.Frame address address value)
  LookupLinksError :: address ->  HeapError address (Map EdgeLabel (Map address address))
  LookupLinkError :: Path address ->  HeapError address address

deriving instance Eq address => Eq (HeapError address resume)
deriving instance Show address => Show (HeapError address resume)
instance Show address => Show1 (HeapError address) where
  liftShowsPrec _ _ = showsPrec

instance Eq address => Eq1 (HeapError address) where
  liftEq _ CurrentFrameError CurrentFrameError           = True
  liftEq _ (LookupAddressError a) (LookupAddressError b) = a == b
  liftEq _ (LookupLinksError a) (LookupLinksError b)     = a == b
  liftEq _ (LookupLinkError a) (LookupLinkError b)       = a == b
  liftEq _ (LookupFrameError a) (LookupFrameError b)     = a == b
  liftEq _ _ _                                           = False

throwHeapError  :: ( Has (Resumable (BaseError (HeapError address))) sig m
                   , Has (Reader ModuleInfo) sig m
                   , Has (Reader Span) sig m
                   )
                => HeapError address resume
                -> Evaluator term address value m resume
throwHeapError = throwBaseError

runHeapError :: Evaluator term address value (Either.ResumableC (BaseError (HeapError address)) m) a
             -> Evaluator term address value m (Either (SomeError (BaseError (HeapError address))) a)
runHeapError = raiseHandler Either.runResumable

runHeapErrorWith :: (forall resume. (BaseError (HeapError address)) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (With.ResumableC (BaseError (HeapError address)) m) a
                 -> Evaluator term address value m a
runHeapErrorWith f = raiseHandler $ With.runResumable (runEvaluator . f)

data AddressError address value resume where
  UnallocatedSlot   :: Slot address -> AddressError address value (Set value)
  UninitializedSlot :: Slot address -> AddressError address value value

deriving instance Eq address => Eq (AddressError address value resume)
deriving instance Show address => Show (AddressError address value resume)
instance Show address => Show1 (AddressError address value) where
  liftShowsPrec _ _ = showsPrec
instance Eq address => Eq1 (AddressError address value) where
  liftEq _ (UninitializedSlot a) (UninitializedSlot b) = a == b
  liftEq _ (UnallocatedSlot a)   (UnallocatedSlot b)   = a == b
  liftEq _ _                        _                  = False

throwAddressError :: ( Has (Resumable (BaseError (AddressError address body))) sig m
                     , Has (Reader ModuleInfo) sig m
                     , Has (Reader Span) sig m
                     )
                  => AddressError address body resume
                  -> Evaluator term address value m resume
throwAddressError = throwBaseError

runAddressError :: Evaluator term address value (Either.ResumableC (BaseError (AddressError address value)) m) a
                -> Evaluator term address value m (Either (SomeError (BaseError (AddressError address value))) a)
runAddressError = raiseHandler Either.runResumable

runAddressErrorWith :: (forall resume . (BaseError (AddressError address value)) resume -> Evaluator term address value m resume)
                    -> Evaluator term address value (With.ResumableC (BaseError (AddressError address value)) m) a
                    -> Evaluator term address value m a
runAddressErrorWith f = raiseHandler $ With.runResumable (runEvaluator . f)
