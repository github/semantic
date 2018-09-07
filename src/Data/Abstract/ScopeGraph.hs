{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}
module Data.Abstract.ScopeGraph
  ( ScopeGraph
  , Path
  ,  Reference
  ,  Declaration
  ,  EdgeLabel
  ,  Heap
  , frameLookup
  , scopeLookup
  , frameSlots
  , frameLinks
  , getSlot
  , setSlot
  , lookup
  ) where

import Data.Abstract.Live
import qualified Data.Map.Monoidal as Monoidal
import qualified Data.Map.Strict as Map
import Data.Semigroup.Reducer
import Prologue
import Prelude hiding (lookup)

data Scope scopeAddress name term ddata = Scope {
    edges :: Map EdgeLabel [scopeAddress]
  , references :: Map (Reference name term) (Path scopeAddress name term)
  , declarations :: Map (Declaration name term) ddata
}

newtype ScopeGraph scopeAddress name term ddata = ScopeGraph { unScopeGraph :: Map scopeAddress (Scope scopeAddress name term ddata) }

data Path scopeAddress name term where
  DPath :: Declaration name term -> Path scopeAddress name term
  EPath :: EdgeLabel -> scopeAddress -> (Path scopeAddress name syntax) -> Path scopeAddress name term

data Reference name term = Reference name term

data Declaration name term = Declaration name term

data EdgeLabel = P | I
  deriving (Eq, Ord, Show)

data Frame scopeAddress frameAddress declaration value = Frame {
    scopeAddress :: scopeAddress
  , links :: Map EdgeLabel (Map scopeAddress frameAddress)
  , slots :: Map declaration value
  }

newtype Heap scopeAddress frameAddress declaration value = Heap { unHeap :: Monoidal.Map frameAddress (Frame scopeAddress frameAddress declaration value) }

-- | Look up the frame for an 'address' in a 'Heap', if any.
frameLookup :: Ord address => address -> Heap scope address declaration value -> Maybe (Frame scope address declaration value)
frameLookup address = Monoidal.lookup address . unHeap

-- | Look up the scope address for a given frame address.
scopeLookup :: Ord address => address -> Heap scope address declaration value -> Maybe scope
scopeLookup address = fmap scopeAddress . frameLookup address

frameSlots :: Ord address => address -> Heap scope address declaration value -> Maybe (Map declaration value)
frameSlots address = fmap slots . frameLookup address

frameLinks :: Ord address => address -> Heap scope address declaration value -> Maybe (Map EdgeLabel (Map scope address))
frameLinks address = fmap links . frameLookup address

getSlot :: (Ord address, Ord declaration) => address -> Heap scope address declaration value -> declaration -> Maybe value
getSlot address heap declaration = do
  slotMap <- frameSlots address heap
  Map.lookup declaration slotMap

setSlot :: (Ord address, Ord declaration) => address -> declaration -> value -> Heap scope address declaration value -> Heap scope address declaration value
setSlot address declaration value heap =
    case frameLookup address heap of
      Just frame -> let slotMap = slots frame in
        Heap $ Monoidal.insert address (frame { slots = (Map.insert declaration value slotMap) }) (unHeap heap)
      Nothing -> heap

lookup :: (Ord address, Ord scope) => Heap scope address declaration value -> address -> (Path scope name term) -> declaration -> Maybe scope
lookup heap address (DPath d) declaration = scopeLookup address heap
lookup heap address (EPath label scope path) declaration = do
    frame <- frameLookup address heap
    scopeMap <- Map.lookup label (links frame)
    nextAddress <- Map.lookup scope scopeMap
    lookup heap nextAddress path declaration

newFrame :: (Ord address, Ord declaration) => scope -> address -> Map EdgeLabel (Map scope address) -> Heap scope address declaration value -> Heap scope address declaration value
newFrame scope address links = insertFrame address (Frame scope links mempty)

initFrame :: (Ord address, Ord declaration, Ord scope) => scope -> address -> Map EdgeLabel (Map scope address) -> Map declaration value -> Heap scope address declaration value -> Heap scope address declaration value
initFrame scope address links slots = fillFrame address slots . newFrame scope address links

insertFrame :: Ord address => address -> Frame scope address declaration value -> Heap scope address declaration value -> Heap scope address declaration value
insertFrame address frame = Heap . Monoidal.insert address frame . unHeap

fillFrame :: Ord address => address -> Map declaration value -> Heap scope address declaration value -> Heap scope address declaration value
fillFrame address slots heap =
  case frameLookup address heap of
    Just frame -> insertFrame address (frame { slots = slots }) heap
    Nothing -> heap

-- -- | Look up the list of values stored for a given address, if any.
-- scopeLookupAll :: Ord address => address -> Heap address value -> Maybe [value]
-- scopeLookupAll address = fmap toList . scopeLookup address

-- -- | Append a value onto the cell for a given address, inserting a new cell if none existed.
-- scopeInsert :: (Ord address, Ord value) => address -> value -> Scope address value -> Scope address value
-- scopeInsert address value = flip snoc (address, value)

-- -- | Manually insert a cell into the scope at a given address.
-- scopeInit :: Ord address => address -> Set value -> Scope address value -> Scope address value
-- scopeInit address cell (Scope h) = Scope (Monoidal.insert address cell h)

-- -- | The number of addresses extant in a 'Scope'.
-- scopeSize :: Scope address value -> Int
-- scopeSize = Monoidal.size . unScope

-- -- | Restrict a 'Scope' to only those addresses in the given 'Live' set (in essence garbage collecting the rest).
-- scopeRestrict :: Ord address => Scope address value -> Live address -> Scope address value
-- scopeRestrict (Scope m) roots = Scope (Monoidal.filterWithKey (\ address _ -> address `liveMember` roots) m)

-- scopeDelete :: Ord address => address -> Scope address value -> Scope address value
-- scopeDelete addr = Scope . Monoidal.delete addr . unScope

-- instance (Ord address, Ord value) => Reducer (address, value) (Scope address value) where
--   unit = Scope . unit
--   cons (addr, a) (Scope scope) = Scope (cons (addr, a) scope)
--   snoc (Scope scope) (addr, a) = Scope (snoc scope (addr, a))

-- instance (Show address, Show value) => Show (Scope address value) where
--   showsPrec d = showsUnaryWith showsPrec "Scope" d . map (second toList) . Monoidal.pairs . unScope
