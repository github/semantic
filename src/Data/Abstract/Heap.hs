{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveAnyClass #-}
module Data.Abstract.Heap
  ( Heap(..)
  , Frame(..)
  , frameLookup
  , scopeLookup
  , frameSlots
  , frameLinks
  , getSlot
  , setSlot
  , deleteSlot
  , initFrame
  , newFrame
  , heapSize
  , Position(..)
  , pathPosition
  , pathDeclaration
  , lookupFrameAddress
  , lookupDeclaration
  , isHeapEmpty
  ) where

import Data.Abstract.Live
import Data.Abstract.ScopeGraph (EdgeLabel(..), Declaration(..), Path(..), Position(..), Slot(..), ScopeGraph, pathPosition, pathDeclaration, lookupScopePath)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap
import Prologue
import Prelude hiding (lookup)

data Frame scopeAddress frameAddress value = Frame {
    scopeAddress :: scopeAddress
  , links        :: Map EdgeLabel (Map scopeAddress frameAddress)
  , slots        :: IntMap (Set value)
  }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | A map of frame addresses onto Frames.
newtype Heap scopeAddress frameAddress value = Heap { heap :: Map frameAddress (Frame scopeAddress frameAddress value) }
  deriving (Eq, Ord, Generic, NFData, Show)

instance Lower (Heap scopeAddress frameAddress value) where
  lowerBound = Heap lowerBound


-- | Look up the frame for an 'address' in a 'Heap', if any.
frameLookup :: Ord address => address -> Heap scope address value -> Maybe (Frame scope address value)
frameLookup address = Map.lookup address . heap

-- | Look up the scope address for a given frame address.
scopeLookup :: Ord address => address -> Heap scope address value -> Maybe scope
scopeLookup address = fmap scopeAddress . frameLookup address

frameSlots :: Ord address => address -> Heap scope address value -> Maybe (IntMap (Set value))
frameSlots address = fmap slots . frameLookup address

frameLinks :: Ord address => address -> Heap scope address value -> Maybe (Map EdgeLabel (Map scope address))
frameLinks address = fmap links . frameLookup address

getSlot :: Ord address => Slot address -> Heap address address value -> Maybe (Set value)
getSlot Slot{..} = (IntMap.lookup (unPosition position) =<<) . frameSlots frameAddress

setSlot :: Ord address => Slot address -> Set value -> Heap scope address value -> Heap scope address value
setSlot Slot{..} value h@Heap{} =
    case frameLookup frameAddress h of
      Just frame -> let slotMap = slots frame in
        h { heap = Map.insert frameAddress (frame { slots = IntMap.insert (unPosition position) value slotMap }) (heap h) }
      Nothing -> h

deleteSlot :: Ord address => Slot address -> Heap scope address value -> Heap scope address value
deleteSlot Slot{..} h@Heap{} =
    case frameLookup frameAddress h of
      Just frame -> let slotMap = slots frame in
        h { heap = Map.insert frameAddress (frame { slots = IntMap.delete (unPosition position) slotMap }) (heap h) }
      Nothing -> h

lookupDeclaration :: (Ord address, Show address) => Declaration -> (address, address) -> ScopeGraph address -> Heap address address value -> Maybe (Slot address)
lookupDeclaration Declaration{..} (currentScope, currentFrame) scopeGraph heap = do
  path <- lookupScopePath unDeclaration currentScope scopeGraph
  frameAddress <- lookupFrameAddress path  currentFrame heap
  pure (Slot frameAddress (pathPosition path))

lookupFrameAddress :: (Ord address, Ord scope) => Path scope -> address -> Heap scope address value -> Maybe address
lookupFrameAddress path currentFrame h@Heap{..} = do
  go path currentFrame
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
insertFrame address frame h@Heap{..} = h { heap = (Map.insert address frame heap) }

fillFrame :: Ord address => address -> IntMap (Set value) -> Heap scope address value -> Heap scope address value
fillFrame address slots heap =
  case frameLookup address heap of
    Just frame -> insertFrame address (frame { slots = slots }) heap
    Nothing    -> heap

-- | The number of frames in the `Heap`.
heapSize :: Heap scope address value -> Int
heapSize = Map.size . heap

isHeapEmpty :: (Eq address, Eq value) => Heap scope address value -> Bool
isHeapEmpty h@Heap{..} = (heapSize h) == 1 &&
                         (toEmptyFrame <$> Map.elems heap) == [ Frame () mempty mempty ]
  where
    toEmptyFrame Frame{..} = Frame () (Map.mapKeysMonotonic (const ()) <$> links) slots

-- -- | A map of addresses onto cells holding their values.
-- newtype Heap address address value = Heap { unHeap :: Monoidal.Map address (Set value) }
--   deriving (Eq, Foldable, Lower, Monoid, Ord, Semigroup)

-- -- | Look up the cell of values for an 'Address' in a 'Heap', if any.
-- heapLookup :: Ord address => address -> Heap address address value -> Maybe (Set value)
-- heapLookup address = Monoidal.lookup address . unHeap

-- -- | Look up the list of values stored for a given address, if any.
-- heapLookupAll :: Ord address => address -> Heap address address value -> Maybe [value]
-- heapLookupAll address = fmap toList . heapLookup address

-- -- | Append a value onto the cell for a given address, inserting a new cell if none existed.
-- heapInsert :: (Ord address, Ord value) => address -> value -> Heap address address value -> Heap address address value
-- heapInsert address value = flip snoc (address, value)

-- -- | Manually insert a cell into the heap at a given address.
-- heapInit :: Ord address => address -> Set value -> Heap address address value -> Heap address address value
-- heapInit address cell (Heap h) = Heap (Monoidal.insert address cell h)

-- -- | The number of addresses extant in a 'Heap'.
-- heapSize :: Heap address address value -> Int
-- heapSize = Monoidal.size . unHeap

-- -- | Restrict a 'Heap' to only those addresses in the given 'Live' set (in essence garbage collecting the rest).
-- heapRestrict :: Ord address => Heap address address value -> Live address -> Heap address address value
-- heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> address `liveMember` roots) m)

-- heapDelete :: Ord address => address -> Heap address address value -> Heap address address value
-- heapDelete addr = Heap . Monoidal.delete addr . unHeap

-- instance (Ord address, Ord value) => Reducer (address, value) (Heap address address value) where
--   unit = Heap . unit
--   cons (addr, a) (Heap heap) = Heap (cons (addr, a) heap)
--   snoc (Heap heap) (addr, a) = Heap (snoc heap (addr, a))

-- instance (Show address, Show value) => Show (Heap address address value) where
--   showsPrec d = showsUnaryWith showsPrec "Heap" d . map (second toList) . Monoidal.pairs . unHeap
