module Data.Abstract.Frame () where


newtype Heap address = Heap { unScope :: Monoidal.Map address (Frame address) }
  deriving (Eq, Foldable, Lower, Monoid, Ord, Semigroup)

data HeapFrame = HeapFrame { scopeId :: scopeId, []}

newtype Heap scopeId = Heap { unHeap :: [scopeId] }

data Frame address where
    Frame :: address -> HeapTy address -> Frame address

-- setSlot :: t -> value t heap -> Frame s scopes -> Heap scopes -> Heap scopes
-- setSlot d v f h = case lookup h f of
--     Just (slots, links) ->

-- data Frame types scopes where
--     Frame :: Slots types scopes -> Links types scopes -> Frame types scopes
