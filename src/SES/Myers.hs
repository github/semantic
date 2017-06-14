{-# LANGUAGE GADTs, ImplicitParams, MultiParamTypeClasses, ScopedTypeVariables #-}
module SES.Myers
( EditScript
, Step
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
import Prologue hiding (for, error)

-- | An edit script, i.e. a sequence of changes/copies of elements.
type EditScript a b = [These a b]

type Step a b = State (MyersState a b)

type Myers a b = (Step a b)

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
    Just (script, _) <- for [0..(n + m)] (searchUpToD . Distance)
    return (reverse script)
  where (n, m) = (length as, length bs)

        -- Search an edit graph for the shortest edit script up to a given proposed edit distance, building on the results of previous searches.
        searchUpToD (Distance d) = for [ k | k <- [negate d, negate d + 2 .. d], inRange (negate m, n) k ] (searchAlongK . Diagonal)
          where -- Search an edit graph for the shortest edit script along a specific diagonal, moving onto a given diagonal from one of its in-bounds adjacent diagonals (if any), and sliding down any diagonal edges eagerly.
                searchAlongK (Diagonal k) = do
                  v <- get
                  let getK k = let (x, script) = v Map.! k in Endpoint x (x - k) script
                      up   = {-# SCC "runSES.searchUpToD.searchAlongK.up" #-} getK (pred k)
                      left = {-# SCC "runSES.searchUpToD.searchAlongK.left" #-} getK (succ k)
                      moveDown  = let Endpoint x y script = up   in Endpoint       x (succ y) (if y < m then That (bs ! y) : script else script)
                      moveRight = let Endpoint x y script = left in Endpoint (succ x)      y  (if x < n then This (as ! x) : script else script)
                      Endpoint x' _ script = slideFrom $! if d == 0 || k < negate m || k > n then
                        -- The top-left corner, or otherwise out-of-bounds.
                        Endpoint 0 0 []
                      else if k == negate d || k == negate m then
                        -- The lower/left extent of the search region or edit graph, whichever is smaller.
                        moveDown
                      else if k /= d && k /= n then
                        -- Somewhere in the interior of the search region and edit graph.
                        if x up < x left then
                          moveDown
                        else
                          moveRight
                      else
                        -- The upper/right extent of the search region or edit graph, whichever is smaller.
                        moveRight
                  put ({-# SCC "runSES.searchUpToD.searchAlongK.update" #-} Map.insert k (x', script) v)
                  return $! if x' >= n && (x' - k) >= m then
                    Just (script, d)
                  else
                    Nothing
                  where -- Slide down any diagonal edges from a given vertex.
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

-- | Evaluate some function for each value in a list until one returns a value or the list is exhausted.
for :: [a] -> (a -> Myers c d (Maybe b)) -> Myers c d (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all
{-# INLINE for #-}


-- | Lifted showing of arrays.
liftShowsVector :: Show i => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array.Array i a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList


-- Instances

instance Show2 EditGraph where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (EditGraph as bs) = showsBinaryWith (liftShowsVector sp1 sl1) (liftShowsVector sp2 sl2) "EditGraph" d as bs
