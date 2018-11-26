{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}
module Control.Abstract.Heap
( Heap
, HeapError(..)
, Address(..)
, Position(..)
, Live
, getHeap
, putHeap
, putSlotDeclarationScope
, alloc
, dealloc
, lookupDeclaration
, lookupDeclarationFrame
, deref
, assign
, newFrame
, currentFrame
, withScopeAndFrame
, withLexicalScopeAndFrame
, withChildFrame
, define
, withFrame
, putCurrentFrame
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
, bindFrames
, insertFrameLink
) where

import Control.Abstract.Context (withCurrentCallStack)
import Control.Abstract.Evaluator
import Control.Abstract.Roots
import Control.Applicative (Alternative)
import Control.Effect.Carrier
import Data.Abstract.BaseError
import qualified Data.Abstract.Heap as Heap
import Data.Abstract.ScopeGraph (Path(..), putDeclarationScopeAtPosition)
import Data.Abstract.Heap (Heap, Position(..))
import Control.Abstract.ScopeGraph hiding (ScopeError(..))
import Control.Abstract.ScopeGraph (ScopeError)
import Data.Abstract.Live
import Data.Abstract.Module (ModuleInfo)
import Data.Abstract.Name
import Data.Span (Span)
import qualified Data.Map.Strict as Map
import Prologue
import Data.Abstract.Ref


-- | Evaluates an action locally the scope and frame of the given frame address.
withScopeAndFrame :: forall term address value m a sig. (
                      Ord address
                    , Member (Reader ModuleInfo) sig
                    , Member (Reader Span) sig
                    , Member (Resumable (BaseError (HeapError address))) sig
                    , Member (State (Heap address address value)) sig
                    , Member (State (ScopeGraph address)) sig
                    , Carrier sig m
                    , Show address
                    , Show value
                    )
                  => address
                  -> Evaluator term address value m a
                  -> Evaluator term address value m a
withScopeAndFrame address action = do
  scope <- scopeLookup @address @value address
  withScope scope (withFrame address action)

-- | Evaluates an action locally the scope and frame of the given frame address.
withLexicalScopeAndFrame :: forall term address value m a sig. (
                      Ord address
                    , Member (Reader ModuleInfo) sig
                    , Member (Reader Span) sig
                    , Member (Resumable (BaseError (HeapError address))) sig
                    , Member (Resumable (BaseError (ScopeError address))) sig
                    , Member (State (Heap address address value)) sig
                    , Member (State (ScopeGraph address)) sig
                    , Member (Allocator address) sig
                    , Member Fresh sig
                    , Carrier sig m
                    , Show address
                    , Show value
                    )
                  => Evaluator term address value m a
                  -> Evaluator term address value m a
withLexicalScopeAndFrame action = do
  currentScope' <- currentScope
  currentFrame' <- currentFrame
  let (scopeEdges, frameEdges) = case (currentScope', currentFrame') of
        (Just currentScope', Just currentFrame') ->
          (Map.singleton Lexical [ currentScope' ], Map.singleton Lexical (Map.singleton currentScope' currentFrame'))
        _ -> mempty
  scope <- newScope scopeEdges
  frame <- newFrame scope frameEdges
  withScopeAndFrame frame action

-- | Lookup a scope address for a given frame address.
scopeLookup :: forall address value sig m term. (
                Ord address
              , Member (Reader ModuleInfo) sig
              , Member (Reader Span) sig
              , Member (Resumable (BaseError (HeapError address))) sig
              , Member (State (Heap address address value)) sig
              , Carrier sig m
              )
            => address
            -> Evaluator term address value m address
scopeLookup address = maybeM (throwHeapError (LookupAddressError address)) =<< Heap.scopeLookup address <$> get @(Heap address address value)

getHeap :: (Member (State (Heap address address value)) sig, Carrier sig m) => Evaluator term address value m (Heap address address value)
getHeap = get

-- | Set the heap.
putHeap :: (Member (State (Heap address address value)) sig, Carrier sig m) => Heap address address value -> Evaluator term address value m ()
putHeap = put

-- | Update the heap.
modifyHeap :: (Member (State (Heap address address value)) sig, Carrier sig m) => (Heap address address value -> Heap address address value) -> Evaluator term address value m ()
modifyHeap = modify

-- | Retrieve the heap.
currentFrame :: forall address value sig term m. (
                  Member (State (Heap address address value)) sig
                , Carrier sig m
                )
             => Evaluator term address value m (Maybe address)
currentFrame = Heap.currentFrame <$> get @(Heap address address value)

putCurrentFrame :: forall address value sig m term. ( Member (State (Heap address address value)) sig, Carrier sig m ) => address -> Evaluator term address value m ()
putCurrentFrame address = modify @(Heap address address value) (\heap -> heap { Heap.currentFrame = Just address })

-- | Inserts a new frame into the heap with the given scope and links.
newFrame :: forall address value sig m term. (
            Member (State (Heap address address value)) sig
          , Ord address
          , Member (Allocator address) sig
          , Member Fresh sig
          , Carrier sig m
          )
         => address
         -> Map EdgeLabel (Map address address)
         -> Evaluator term address value m address
newFrame scope links = do
  name <- gensym
  address <- alloc name
  modify @(Heap address address value) (Heap.newFrame scope address links)
  pure address

-- | Evaluates the action within the frame of the given frame address.
withFrame :: forall term address value sig m a. (
             Member (State (Heap address address value)) sig
           , Carrier sig m
           )
          => address
          -> Evaluator term address value m a -- Not sure about this `sig` here (substituting `sig` for `effects`)
          -> Evaluator term address value m a
withFrame address action = do
    prevFrame <- currentFrame @address @value
    modify @(Heap address address value) (\h -> h { Heap.currentFrame = Just address })
    value <- action
    case prevFrame of
      Nothing -> modify @(Heap address address value) (\h -> h { Heap.currentFrame = Just address })
      _ -> modify @(Heap address address value) (\h -> h { Heap.currentFrame = prevFrame })
    pure value

-- box :: ( Member (Allocator address) effects
--        , Member (Deref value) effects
--        , Member Fresh effects
--        , Member (State (Heap address address value)) effects
--        , Ord address
--        )
--     => value
--     -> Evaluator term address value m address
-- box val = do
--   name <- gensym
--   addr <- alloc name
--   assign addr (Declaration name) val -- TODO This is probably wrong
--   pure addr

-- | Define a declaration and assign the value of an action in the current frame.
define :: ( HasCallStack
          , Member (Deref value) sig
          , Member (Reader ModuleInfo) sig
          , Member (Reader Span) sig
          , Member (State (Heap address address value)) sig
          , Member (State (ScopeGraph address)) sig
          , Member (Resumable (BaseError (ScopeError address))) sig
          , Member (Resumable (BaseError (HeapError address))) sig
          , Ord address
          , Show address
          , Carrier sig m
          )
       => Declaration
       -> Evaluator term address value m value
       -> Evaluator term address value m (ValueRef address value)
define declaration def = withCurrentCallStack callStack $ do
  span <- ask @Span -- TODO: This Span is most definitely wrong
  declare declaration span Nothing
  slot <- lookupDeclaration declaration
  value <- def
  LvalMember slot <$ assign slot value

-- | Associate an empty child scope with a declaration and then locally evaluate the body within an associated frame.
withChildFrame :: ( Member (Allocator address) sig
                  , Member (State (Heap address address value)) sig
                  , Member (State (ScopeGraph address)) sig
                  , Member Fresh sig
                  , Member (Reader ModuleInfo) sig
                  , Member (Reader Span) sig
                  , Member (Resumable (BaseError (HeapError address))) sig
                  , Member (Resumable (BaseError (ScopeError address))) sig
                  , Ord address
                  , Carrier sig m
                  , Show address
                  , Show value
                  )
                => Declaration
                -> (address -> Evaluator term address value m a)
                -> Evaluator term address value m a
withChildFrame declaration body = do
  scope <- newScope mempty
  putDeclarationScope declaration scope
  frame <- newFrame scope mempty
  withScopeAndFrame frame (body frame)

-- | Dereference the given address in the heap, or fail if the address is uninitialized.
deref :: ( Member (Deref value) sig
         , Member (Reader ModuleInfo) sig
         , Member (Reader Span) sig
         , Member (Resumable (BaseError (AddressError address value))) sig
         , Member (State (Heap address address value)) sig
         , Ord address
         , Carrier sig m
         )
      => Address address
      -> Evaluator term address value m value
deref slot@Address{..} = gets (Heap.getSlot slot) >>= maybeM (throwAddressError (UnallocatedAddress frameAddress)) >>= send . flip DerefCell ret >>= maybeM (throwAddressError $ UninitializedAddress frameAddress)

putSlotDeclarationScope :: forall address value sig m term. ( Member (State (Heap address address value)) sig
                           , Member (State (ScopeGraph address)) sig
                           , Member (Resumable (BaseError (HeapError address))) sig
                           , Member (Reader ModuleInfo) sig
                           , Member (Reader Span) sig
                           , Ord address
                           , Carrier sig m
                           )
                        => Address address
                        -> Maybe address
                        -> Evaluator term address value m ()
putSlotDeclarationScope Address{..} assocScope = do
  scopeAddress <- scopeLookup frameAddress
  modify @(ScopeGraph address) (putDeclarationScopeAtPosition scopeAddress position assocScope)


lookupDeclaration :: forall value address term sig m. ( Member (State (Heap address address value)) sig
                     , Member (State (ScopeGraph address)) sig
                     , Member (Resumable (BaseError (ScopeError address))) sig
                     , Member (Resumable (BaseError (HeapError address))) sig
                     , Member (Reader ModuleInfo) sig
                     , Member (Reader Span) sig
                     , Ord address
                     , Show address
                     , Carrier sig m
                     )
                  => Declaration
                  -> Evaluator term address value m (Address address)
lookupDeclaration decl = do
  path <- lookupScopePath decl
  frameAddress <- lookupFrameAddress path
  pure (Address frameAddress (Heap.pathPosition path))

lookupDeclarationFrame :: ( Member (State (Heap address address value)) sig
                          , Member (State (ScopeGraph address)) sig
                          , Member (Resumable (BaseError (ScopeError address))) sig
                          , Member (Resumable (BaseError (HeapError address))) sig
                          , Member (Reader ModuleInfo) sig
                          , Member (Reader Span) sig
                          , Ord address
                          , Show address
                          , Carrier sig m
                          )
                       => Declaration
                       -> Evaluator term address value m address
lookupDeclarationFrame decl = do
  path <- lookupScopePath decl
  lookupFrameAddress path

-- | Follow a path through the heap and return the frame address associated with the declaration.
lookupFrameAddress :: ( Member (State (Heap address address value)) sig
                     , Member (Reader ModuleInfo) sig
                     , Member (Reader Span) sig
                     , Member (Resumable (BaseError (HeapError address))) sig
                     , Ord address
                     , Carrier sig m
                     )
                  => Path address
                  -> Evaluator term address value m address
lookupFrameAddress path = do
  frameAddress <- maybeM (throwHeapError CurrentFrameError) =<< currentFrame
  go path frameAddress
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

frameLinks :: forall address value sig m term. (
                Member (State (Heap address address value)) sig
              , Member (Resumable (BaseError (HeapError address))) sig
              , Member (Reader ModuleInfo) sig
              , Member (Reader Span) sig
              , Ord address
              , Carrier sig m
              )
          => address
          -> Evaluator term address value m (Map EdgeLabel (Map address address)) -- TODO: Change this to Map scope address
frameLinks address = maybeM (throwHeapError $ LookupLinksError address) . Heap.frameLinks address =<< get @(Heap address address value)


bindFrames :: ( Ord address
              , Member (State (Heap address address value)) sig
              , Carrier sig m
              )
           => Heap address address value
           -> Evaluator term address value m ()
bindFrames oldHeap = do
  currentHeap <- get
  let newHeap = Heap.heap oldHeap <> Heap.heap currentHeap
  put (currentHeap { Heap.heap = newHeap })


insertFrameLink :: forall address value sig m term. ( Member (State (Heap address address value)) sig
                   , Member (Resumable (BaseError (HeapError address))) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Ord address
                   , Carrier sig m
                   )
                => EdgeLabel -> Map address address -> Evaluator term address value m ()
insertFrameLink label linkMap = do
  frameAddress <- maybeM (throwHeapError CurrentFrameError) =<< currentFrame
  heap <- get @(Heap address address value)
  currentFrame <- maybeM (throwHeapError $ LookupFrameError frameAddress) (Heap.frameLookup frameAddress heap)
  let newCurrentFrame = currentFrame {
        Heap.links = Map.alter (\val -> val <> Just linkMap) label (Heap.links currentFrame)
      }
  modify (\h -> h { Heap.heap = Map.insert frameAddress newCurrentFrame (Heap.heap h)})


-- | Write a value to the given frame address in the 'Heap'.
assign :: ( Member (Deref value) sig
          , Member (State (Heap address address value)) sig
          , Ord address
          , Carrier sig m
          )
       => Address address
       -> value
       -> Evaluator term address value m ()
assign addr value = do
  heap <- getHeap
  cell <- send (AssignCell value (fromMaybe lowerBound (Heap.getSlot addr heap)) ret)
  putHeap (Heap.setSlot addr cell heap)

dealloc :: forall address value sig m term. ( Member (Deref value) sig
          , Member (State (Heap address address value)) sig
          , Ord address
          , Carrier sig m
          )
       => Address address
       -> Evaluator term address value m ()
dealloc addr = modify @(Heap address address value) (Heap.deleteSlot addr)


-- Garbage collection

-- | Collect any addresses in the heap not rooted in or reachable from the given 'Live' set.
-- gc :: ( Member (State (Heap address address value)) sig
--       , Ord address
--       , ValueRoots address value
--       , Carrier sig m
--       )
--    => Live address                       -- ^ The set of addresses to consider rooted.
--    -> Evaluator term address value m ()
gc :: Live address                       -- ^ The set of addresses to consider rooted.
   -> Evaluator term address value m ()
-- gc roots =
gc _ =
  -- TODO: Implement frame garbage collection
  undefined
  -- modifyHeap (heapRestrict <*> reachable roots)

-- | Compute the set of addresses reachable from a given root set in a given heap.
reachable :: Ord address
          => Live address       -- ^ The set of root addresses.
          -> Heap address address value -- ^ The heap to trace addresses through.
          -> Live address       -- ^ The set of addresses reachable from the root set.
reachable roots _ = go mempty roots
  where go seen set = case liveSplit set of
          Nothing -> seen
          Just (_, _) -> undefined -- go (liveInsert a seen) $ case heapLookupAll a heap of
            -- Just values -> liveDifference (foldr ((<>) . valueRoots) mempty values <> as) seen
            -- _           -> seen


-- Effects

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

runDeref :: Carrier (Deref value :+: sig) (DerefC address value (Eff m))
         => Evaluator term address value (DerefC address value (Eff m)) a
         -> Evaluator term address value m a
runDeref = raiseHandler $ runDerefC . interpret

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
  liftEq _ CurrentFrameError CurrentFrameError = True
  liftEq _ (LookupAddressError a) (LookupAddressError b) = a == b
  liftEq _ (LookupLinksError a) (LookupLinksError b) = a == b
  liftEq _ (LookupLinkError a) (LookupLinkError b) = a == b
  liftEq _ (LookupFrameError a) (LookupFrameError b) = a == b
  liftEq _ _ _ = False

throwHeapError  :: ( Member (Resumable (BaseError (HeapError address))) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Span) sig
                   , Carrier sig m
                   )
                => HeapError address resume
                -> Evaluator term address value m resume
throwHeapError = throwBaseError

runHeapError :: (Carrier sig m, Effect sig)
                => Evaluator term address value (ResumableC (BaseError (HeapError address)) (Eff m)) a
                -> Evaluator term address value m (Either (SomeError (BaseError (HeapError address))) a)
runHeapError = raiseHandler runResumable

runHeapErrorWith :: Carrier sig m
                 => (forall resume. (BaseError (HeapError address)) resume -> Evaluator term address value m resume)
                 -> Evaluator term address value (ResumableWithC (BaseError (HeapError address)) (Eff m)) a
                 -> Evaluator term address value m a
runHeapErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)

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
                => Evaluator term address value (ResumableC (BaseError (AddressError address value)) (Eff m)) a
                -> Evaluator term address value m (Either (SomeError (BaseError (AddressError address value))) a)
runAddressError = raiseHandler runResumable

runAddressErrorWith :: Carrier sig m
                    => (forall resume . (BaseError (AddressError address value)) resume -> Evaluator term address value m resume)
                    -> Evaluator term address value (ResumableWithC (BaseError (AddressError address value)) (Eff m)) a
                    -> Evaluator term address value m a
runAddressErrorWith f = raiseHandler $ runResumableWith (runEvaluator . f)
