{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Data.Abstract.Heap
  ( Heap(..)
  , Frame(..)
  , frameLookup
  , scopeLookup
  , frameSlots
  , frameLinks
  , getSlotValue
  , setSlot
  , deleteSlot
  , initFrame
  , newFrame
  , insertFrame
  , heapLookup
  , heapLookupAll
  , heapSize
  , heapRestrict
  , Position(..)
  , pathPosition
  , pathDeclaration
  , lookupFrameAddress
  , lookupDeclaration
  , isHeapEmpty
  ) where

import           Data.Abstract.Live
import           Data.Abstract.ScopeGraph
    ( Declaration (..)
    , EdgeLabel (..)
    , Path (..)
    , Position (..)
    , ScopeGraph
    , Slot (..)
    , lookupScopePath
    , pathDeclaration
    , pathPosition
    )
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import           Prelude hiding (lookup)
import           Prologue

-- | A Frame describes the vertices of the Heap. Think of it as an instance of a Scope in the ScopeGraph.
data Frame scopeAddress frameAddress value = Frame
  { scopeAddress :: scopeAddress
    -- ^ The scope address of its corresponding Scope
  , links        :: Map EdgeLabel (Map scopeAddress frameAddress)
  -- ^ A map of edge labels to maps of scope addresses to frame addresses (scope addresses are unique keys in the ScopeGraph
  --   and frame addresses are unique keys in the Heap). This map describes the frame’s links to other frames.
  --   A frame’s links are always in one-to-one correspondence with its scope’s edges to parent scopes.
  , slots        :: IntMap (Set value)
  -- ^ An IntMap of values that are declared in the frame.
  }
  deriving (Eq, Ord, Show)

-- | A Heap is a Map from frame addresses to frames.
newtype Heap scopeAddress frameAddress value = Heap { unHeap :: Map frameAddress (Frame scopeAddress frameAddress value) }
  deriving (Eq, Lower, Ord)


-- | Look up the frame for an 'address' in a 'Heap', if any.
frameLookup :: Ord address => address -> Heap scope address value -> Maybe (Frame scope address value)
frameLookup address = Map.lookup address . unHeap

-- | Look up the scope address for a given frame address.
scopeLookup :: Ord address => address -> Heap scope address value -> Maybe scope
scopeLookup address = fmap scopeAddress . frameLookup address

frameSlots :: Ord address => address -> Heap scope address value -> Maybe (IntMap (Set value))
frameSlots address = fmap slots . frameLookup address

frameLinks :: Ord address => address -> Heap scope address value -> Maybe (Map EdgeLabel (Map scope address))
frameLinks address = fmap links . frameLookup address

getSlotValue :: Ord address => Slot address -> Heap address address value -> Maybe (Set value)
getSlotValue Slot{..} = (IntMap.lookup (unPosition position) =<<) . frameSlots frameAddress

setSlot :: Ord address => Slot address -> Set value -> Heap scope address value -> Heap scope address value
setSlot Slot{..} value h@(Heap heap) = case frameLookup frameAddress h of
  Just frame -> let slotMap = slots frame in
    Heap (Map.insert frameAddress (frame { slots = IntMap.insert (unPosition position) value slotMap }) heap)
  Nothing -> h

deleteSlot :: Ord address => Slot address -> Heap scope address value -> Heap scope address value
deleteSlot Slot{..} h@(Heap heap) = case frameLookup frameAddress h of
  Just frame -> let slotMap = slots frame in
    Heap (Map.insert frameAddress (frame { slots = IntMap.delete (unPosition position) slotMap }) heap)
  Nothing -> h

lookupDeclaration :: Ord address
                  => Declaration
                  -> (address, address)
                  -> ScopeGraph address
                  -> Heap address address value
                  -> Maybe (Slot address)
lookupDeclaration Declaration{..} (currentScope, currentFrame) scopeGraph heap = do
  path <- lookupScopePath unDeclaration currentScope scopeGraph
  frameAddress <- lookupFrameAddress path  currentFrame heap
  pure (Slot frameAddress (pathPosition path))

lookupFrameAddress :: (Ord address, Ord scope) => Path scope -> address -> Heap scope address value -> Maybe address
lookupFrameAddress path currentFrame h = go path currentFrame
  where
    go path address = case path of
      DPath _ _ -> pure address
      EPath edge nextScopeAddress path' -> do
        linkMap <- frameLinks address h
        frameAddress <- do
          scopeMap <- Map.lookup edge linkMap
          Map.lookup nextScopeAddress scopeMap
        go path' frameAddress
      Hole -> Nothing

newFrame :: (Ord address) => scope -> address -> Map EdgeLabel (Map scope address) -> Heap scope address value -> Heap scope address value
newFrame scope address links = insertFrame address (Frame scope links mempty)

initFrame :: (Ord address) => scope -> address -> Map EdgeLabel (Map scope address) -> IntMap (Set value) -> Heap scope address value -> Heap scope address value
initFrame scope address links slots = fillFrame address slots . newFrame scope address links

insertFrame :: Ord address => address -> Frame scope address value -> Heap scope address value -> Heap scope address value
insertFrame address frame = Heap . Map.insert address frame . unHeap

fillFrame :: Ord address => address -> IntMap (Set value) -> Heap scope address value -> Heap scope address value
fillFrame address slots heap = case frameLookup address heap of
  Just frame -> insertFrame address (frame { slots = slots }) heap
  Nothing    -> heap

-- | Look up the cell of values for an address in a 'Heap', if any.
heapLookup :: (Ord address, Ord value) => address -> Heap address address value -> Maybe (Set value)
heapLookup address = fmap (fold . IntMap.elems . slots) . Map.lookup address . unHeap

-- | Look up the list of values stored for a given address, if any.
heapLookupAll :: (Ord address, Ord value) => address -> Heap address address value -> Maybe [value]
heapLookupAll address = fmap toList . heapLookup address

-- | The number of frames in the `Heap`.
heapSize :: Heap scope address value -> Int
heapSize = Map.size . unHeap

-- | Restrict a 'Heap' to only those addresses in the given 'Live' set (in essence garbage collecting the rest).
heapRestrict :: Ord address => Heap address address value -> Live address -> Heap address address value
heapRestrict (Heap m) roots = Heap (Map.filterWithKey (\ address _ -> address `liveMember` roots) m)

-- | Checks whether a heap as no slots and links.
isHeapEmpty :: (Eq address, Eq value) => Heap scope address value -> Bool
isHeapEmpty h@(Heap heap)
  =  heapSize h == 1
  && (toEmptyFrame <$> Map.elems heap) == [ Frame () mempty mempty ]
  where
    -- Maps a frame's address param to () so we can check that its slots and links are empty.
    toEmptyFrame Frame{..} = Frame () (Map.mapKeysMonotonic (const ()) <$> links) slots


instance (Show address, Show value) => Show (Heap address address value) where
  showsPrec d = showsUnaryWith showsPrec "Heap" d . Map.toList . unHeap
