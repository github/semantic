{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module SES.Myers
( EditScript
, EditGraph(..)
, Diagonal(..)
, Endpoint(..)
, ses
, MyersState
) where

import Data.Array ((!))
import qualified Data.Array as Array
import qualified Data.IntMap.Lazy as Map
import Data.Ix
import Data.Functor.Classes
import Data.These
import GHC.Show hiding (show)
import Prologue hiding (error)

-- | An edit script, i.e. a sequence of changes/copies of elements.
type EditScript a b = [These a b]

-- | Notionally the cartesian product of two sequences, represented as a simple wrapper around those arrays holding those sequences’ elements for O(1) lookups.
data EditGraph a b = EditGraph { as :: !(Array.Array Int a), bs :: !(Array.Array Int b) }
  deriving (Eq, Show)

-- | Construct an edit graph from Foldable sequences.
makeEditGraph :: (Foldable t, Foldable u) => t a -> u b -> EditGraph a b
makeEditGraph as bs = EditGraph (Array.listArray (0, pred (length as)) (toList as)) (Array.listArray (0, pred (length bs)) (toList bs))

-- | A diagonal in the edit graph of lists of lengths n and m, numbered from -m to n.
newtype Diagonal = Diagonal { unDiagonal :: Int }
  deriving (Eq, Ix, Ord, Show)

data Endpoint a b = Endpoint { x :: {-# UNPACK #-} !Int, y :: {-# UNPACK #-} !Int, script :: !(EditScript a b) }
  deriving (Eq, Show)


-- API

-- | Compute the shortest edit script using Myers’ algorithm.
ses :: (Foldable t, Foldable u) => (a -> b -> Bool) -> t a -> u b -> EditScript a b
ses eq as bs = let graph = makeEditGraph as bs in runSES eq graph


-- Evaluation

-- | Compute the shortest edit script (diff) of an edit graph.
runSES :: (a -> b -> Bool) -> EditGraph a b -> EditScript a b
runSES eq (EditGraph as bs)
  | null bs = This <$> toList as
  | null as = That <$> toList bs
  | otherwise = reverse (searchUpToD [0..(n + m)] (Map.singleton 0 (0, [])))
  where (n, m) = (length as, length bs)

        -- Search an edit graph for the shortest edit script up to a given proposed edit distance, building on the results of previous searches.
        searchUpToD (d:ds) v =
          let endpoints = searchAlongK v . Diagonal <$> [ k | k <- [negate d, negate d + 2 .. d], inRange (negate m, n) k ] in
          case find isComplete endpoints of
            Just (Endpoint _ _ script) -> script
            _ -> searchUpToD ds (Map.fromList ((\ (Endpoint x y script) -> (x - y, (x, script))) <$> endpoints))
          where isComplete (Endpoint x y _) = x >= n && y >= m

                -- Search an edit graph for the shortest edit script along a specific diagonal, moving onto a given diagonal from one of its in-bounds adjacent diagonals (if any), and sliding down any diagonal edges eagerly.
                searchAlongK v (Diagonal k) = slideFrom $!
                  if d == 0 || k < negate m || k > n then
                    -- The top-left corner, or otherwise out-of-bounds.
                    Endpoint 0 0 []
                  else if k == negate d || k == negate m then
                    -- The lower/left extent of the search region or edit graph, whichever is smaller.
                    moveDownFrom next
                  else if k /= d && k /= n then
                    -- Somewhere in the interior of the search region and edit graph.
                    if x prev < x next then
                      moveDownFrom next
                    else
                      moveRightFrom prev
                  else
                    -- The upper/right extent of the search region or edit graph, whichever is smaller.
                    moveRightFrom prev
                  where getK k = let (x, script) = v Map.! k in Endpoint x (x - k) script
                        prev = getK (pred k)
                        next = getK (succ k)

                -- | Move downward from a given vertex, inserting the element for the corresponding row.
                moveDownFrom (Endpoint x y script) = Endpoint x (succ y) (if y < m then That (bs ! y) : script else script)

                -- | Move rightward from a given vertex, deleting the element for the corresponding column.
                moveRightFrom (Endpoint x y script) = Endpoint (succ x) y (if x < n then This (as ! x) : script else script)

                -- | Slide down any diagonal edges from a given vertex.
                slideFrom (Endpoint x y script)
                  | x >= 0, x < n
                  , y >= 0, y < m
                  , a <- as ! x
                  , b <- bs ! y
                  , a `eq` b  = slideFrom (Endpoint (succ x) (succ y) (These a b : script))
                  | otherwise =           (Endpoint       x        y               script)


-- Implementation details

-- | The state stored by Myers’ algorithm; an array of m + n + 1 values indicating the maximum x-index reached and path taken along each diagonal.
type MyersState a b = Map.IntMap (Int, EditScript a b)


-- | Lifted showing of arrays.
liftShowsVector :: Show i => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array.Array i a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList


-- Instances

instance Show2 EditGraph where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (EditGraph as bs) = showsBinaryWith (liftShowsVector sp1 sl1) (liftShowsVector sp2 sl2) "EditGraph" d as bs
