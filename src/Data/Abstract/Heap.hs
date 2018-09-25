{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Abstract.Heap
  ( Heap
  , frameLookup
  , scopeLookup
  , frameSlots
  , frameLinks
  , getSlot
  , setSlot
  , initFrame
  , newFrame
  , heapSize
  , currentFrame
  , Position(..)
  ) where

import Data.Abstract.Live
import Data.Abstract.ScopeGraph (EdgeLabel(..), Declaration(..), Path(..), Position(..), Address(..))
import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap
import qualified Data.Map.Monoidal as Monoidal
import Data.Semigroup.Reducer
import Prologue
import Prelude hiding (lookup)


data Frame scopeAddress frameAddress value = Frame {
    scopeAddress :: scopeAddress
  , links        :: Map EdgeLabel (Map scopeAddress frameAddress)
  , slots        :: IntMap (Set value)
  }
  deriving (Eq, Ord, Show)

data Heap scopeAddress frameAddress value = Heap { currentFrame :: Maybe frameAddress, heap :: Map frameAddress (Frame scopeAddress frameAddress value) }
    deriving (Eq, Ord, Show)

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

getSlot :: Ord address => Address address -> Heap address address value -> Maybe (Set value)
getSlot Address{..} = (IntMap.lookup (unPosition position) =<<) . frameSlots address

setSlot :: Ord address => Address address -> Set value -> Heap scope address value -> Heap scope address value
setSlot Address{..} value h@Heap{} =
    case frameLookup address h of
      Just frame -> let slotMap = slots frame in
        h { heap = Map.insert address (frame { slots = IntMap.insert (unPosition position) value slotMap }) (heap h) }
      Nothing -> h

lookup :: (Ord address, Ord scope) => Heap scope address value -> address -> Path scope -> Declaration -> Maybe scope
lookup heap address (DPath d _) declaration = guard (d == declaration) >> scopeLookup address heap
lookup heap address (EPath label scope path) declaration = do
    frame <- frameLookup address heap
    scopeMap <- Map.lookup label (links frame)
    nextAddress <- Map.lookup scope scopeMap
    lookup heap nextAddress path declaration

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

deleteFrame :: Ord address => address -> Heap scope address value -> Heap scope address value
deleteFrame address h@Heap{..} = h { heap = Map.delete address heap }

-- | The number of frames in the `Heap`.
heapSize :: Heap scope address value -> Int
heapSize = Map.size . heap

-- -- | A map of addresses onto cells holding their values.
-- newtype Heap address value = Heap { unHeap :: Monoidal.Map address (Set value) }
--   deriving (Eq, Foldable, Lower, Monoid, Ord, Semigroup)

-- -- | Look up the cell of values for an 'Address' in a 'Heap', if any.
-- heapLookup :: Ord address => address -> Heap address value -> Maybe (Set value)
-- heapLookup address = Monoidal.lookup address . unHeap

-- -- | Look up the list of values stored for a given address, if any.
-- heapLookupAll :: Ord address => address -> Heap address value -> Maybe [value]
-- heapLookupAll address = fmap toList . heapLookup address

-- -- | Append a value onto the cell for a given address, inserting a new cell if none existed.
-- heapInsert :: (Ord address, Ord value) => address -> value -> Heap address value -> Heap address value
-- heapInsert address value = flip snoc (address, value)

-- -- | Manually insert a cell into the heap at a given address.
-- heapInit :: Ord address => address -> Set value -> Heap address value -> Heap address value
-- heapInit address cell (Heap h) = Heap (Monoidal.insert address cell h)

-- -- | The number of addresses extant in a 'Heap'.
-- heapSize :: Heap address value -> Int
-- heapSize = Monoidal.size . unHeap

-- -- | Restrict a 'Heap' to only those addresses in the given 'Live' set (in essence garbage collecting the rest).
-- heapRestrict :: Ord address => Heap address value -> Live address -> Heap address value
-- heapRestrict (Heap m) roots = Heap (Monoidal.filterWithKey (\ address _ -> address `liveMember` roots) m)

-- heapDelete :: Ord address => address -> Heap address value -> Heap address value
-- heapDelete addr = Heap . Monoidal.delete addr . unHeap

-- instance (Ord address, Ord value) => Reducer (address, value) (Heap address value) where
--   unit = Heap . unit
--   cons (addr, a) (Heap heap) = Heap (cons (addr, a) heap)
--   snoc (Heap heap) (addr, a) = Heap (snoc heap (addr, a))

-- instance (Show address, Show value) => Show (Heap address value) where
--   showsPrec d = showsUnaryWith showsPrec "Heap" d . map (second toList) . Monoidal.pairs . unHeap
