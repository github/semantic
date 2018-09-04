{-# LANGUAGE TypeOperators, PolyKinds, GADTs, TypeFamilies, UndecidableInstances, RankNTypes #-}
module Scope (declsOf, edgesOf, Scope, Graph) where

import Data.Kind (Type)
import Data.Finite
import GHC.TypeLits

-- Scope Graphs
-- Type aliases for scope identifiers (`Scope`) and scope graphs (`Graph`).
type Scope k = Finite k

newtype Graph ty k = Graph { unGraph :: Scope k -> ([ty], [Scope k]) }

declsOf :: Graph ty k -> Scope k -> [ty]
declsOf g = fst . unGraph g

edgesOf :: Graph ty k -> Scope k -> [Scope k]
edgesOf g = snd . unGraph g

type family Fst (k :: (m, n)) where
    Fst '(a, b) = a

-- type family EdgesOf (g :: Graph ty (k :: 'Nat)) (s :: y) :: [y] where
--   EdgesOf g s = Fst (g s)

data (––>) x y where
    Empty :: s ––> s
    -- TODO: `s` here was previously `Member s' (EdgesOf s)`
    Cons  :: s -> s' ––> s'' -> s ––> s''

concat :: s ––> s' -> s' ––> s'' -> s ––> s''
concat Empty s2 = s2
concat (Cons a s1) s2 = (Cons a (Scope.concat s1 s2))

data (|>) s name where
    Path :: s ––> s' -> name  -> s |> name

prepend :: s ––> s' -> s' |> name  -> s |> name
prepend p (Path p' name) = Path (Scope.concat p p') name

type HeapTy address = [address]

type HeapTy k = [Scope k]

data FramePtr address where
    FramePtr :: address -> HeapTy address -> FramePtr address

data Slots ty address where
    Slots :: [ty] -> HeapTy address -> Slots ty address

data Links address where
    Links :: [address] -> HeapTy address -> Links address

data HeapFrame address where
    HeapFrame :: Scope address -> HeapTy address -> HeapFrame address

data Heap address where
    Heap :: HeapTy address -> Heap address


-- Store frames in the heap
-- Lookup frame in the scope graph for a resolution path
-- Apply resolution path in the heap to get the value of a probably well-typed address.
-- Environment may be subsumed by a Reader of the current scope in the scope graph.
-- The scope graph is probably the primary data structure.
-- We shouldn't be concerned with the shapes of the frames as long as they correspond to the correct side-effects.


-- Figure out how to look up names in the scope graph


-- data Scope m a where
--     GetFrame :: s ––> s' -> Frame s e -> Heap e -> Scope m (Frame s' e)
