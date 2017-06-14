{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses, ScopedTypeVariables #-}
module SES.Myers
( EditScript
, Myers
, EditGraph(..)
, Distance(..)
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

type Myers a b = State (MyersState a b)

-- | Notionally the cartesian product of two sequences, represented as a simple wrapper around those arrays holding those sequences’ elements for O(1) lookups.
data EditGraph a b = EditGraph { as :: !(Array.Array Int a), bs :: !(Array.Array Int b) }
  deriving (Eq, Show)

-- | Construct an edit graph from Foldable sequences.
makeEditGraph :: (Foldable t, Foldable u) => t a -> u b -> EditGraph a b
makeEditGraph as bs = EditGraph (Array.listArray (0, pred (length as)) (toList as)) (Array.listArray (0, pred (length bs)) (toList bs))

-- | An edit distance, i.e. a cardinal number of changes.
newtype Distance = Distance { unDistance :: Int }
  deriving (Eq, Show)

-- | A diagonal in the edit graph of lists of lengths n and m, numbered from -m to n.
newtype Diagonal = Diagonal { unDiagonal :: Int }
  deriving (Eq, Ix, Ord, Show)

data Endpoint a b = Endpoint { x :: !Int, y :: !Int, script :: !(EditScript a b) }
  deriving (Eq, Show)


-- API

-- | Compute the shortest edit script using Myers’ algorithm.
ses :: (Foldable t, Foldable u) => (a -> b -> Bool) -> t a -> u b -> EditScript a b
ses eq as bs = let graph = makeEditGraph as bs in evalState (runSES eq graph) (emptyStateForGraph graph)


-- Evaluation

-- | Compute the shortest edit script (diff) of an edit graph.
runSES :: (a -> b -> Bool) -> EditGraph a b -> Myers a b (EditScript a b)
runSES eq (EditGraph as bs)
  | null bs = return (This <$> toList as)
  | null as = return (That <$> toList bs)
  | otherwise = do
    Just script <- asum <$> for [0..(n + m)] (searchUpToD . Distance)
    return (reverse script)
  where (n, m) = (length as, length bs)

        -- Search an edit graph for the shortest edit script up to a given proposed edit distance, building on the results of previous searches.
        searchUpToD (Distance d) = do
          v <- get
          let extents = searchAlongK v . Diagonal <$> [ k | k <- [negate d, negate d + 2 .. d], inRange (negate m, n) k ]
          put (Map.fromList extents)
          pure . fmap (snd . snd) $! find isComplete extents
          where isComplete (k, (x, _)) = x >= n && (x - k) >= m

                -- Search an edit graph for the shortest edit script along a specific diagonal, moving onto a given diagonal from one of its in-bounds adjacent diagonals (if any), and sliding down any diagonal edges eagerly.
                searchAlongK v (Diagonal k) =
                  let Endpoint x' _ script = slideFrom $! if d == 0 || k < negate m || k > n then
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
                  in (k, (x', script))
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
                          , a `eq` b  = slideFrom (Endpoint (succ x)      y (These a b : script))
                          | otherwise =           (Endpoint       x (succ y)             script)


-- Implementation details

-- | The state stored by Myers’ algorithm; an array of m + n + 1 values indicating the maximum x-index reached and path taken along each diagonal.
type MyersState a b = Map.IntMap (Int, EditScript a b)

-- | Compute the empty state of length m + n + 1 for a given edit graph.
emptyStateForGraph :: EditGraph a b -> MyersState a b
emptyStateForGraph _ =
  Map.singleton 0 (0, [])


-- | Lifted showing of arrays.
liftShowsVector :: Show i => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array.Array i a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList


-- Instances

instance Show2 EditGraph where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (EditGraph as bs) = showsBinaryWith (liftShowsVector sp1 sl1) (liftShowsVector sp2 sl2) "EditGraph" d as bs
