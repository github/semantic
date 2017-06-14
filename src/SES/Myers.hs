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
import Data.Ix
import Data.Functor.Classes
import Data.These
import GHC.Show hiding (show)
import Prologue hiding (for, error)
import Text.Show (showListWith)

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

-- | The endpoint of a path through the edit graph, represented as the x/y indices and the script of edits made to get to that point.
data Endpoint a b = Endpoint { x :: !Int, script :: !(EditScript a b) }
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
    Just (script, _) <- for [0..(n + m)] (searchUpToD . Distance)
    return (reverse script)
  where (n, m) = (length as, length bs)

        -- | Search an edit graph for the shortest edit script up to a given proposed edit distance, building on the results of previous searches.
        searchUpToD (Distance d) = for [ k | k <- [negate d, negate d + 2 .. d], inRange (negate m, n) k ] (searchAlongK . Diagonal)
          where -- | Search an edit graph for the shortest edit script along a specific diagonal, moving onto a given diagonal from one of its in-bounds adjacent diagonals (if any), and sliding down any diagonal edges eagerly.
                searchAlongK (Diagonal k) = do
                  v <- get
                  let getK k = let (x, script) = v ! Diagonal k in Endpoint x script
                  let Endpoint x' script = slideFrom $! if d == 0 || k < negate m || k > n then
                        -- The top-left corner, or otherwise out-of-bounds.
                        Endpoint 0 []
                      else if k == negate d || k == negate m then
                        -- The lower/left extent of the search region or edit graph, whichever is smaller.
                        moveDownFrom (getK (succ k))
                      else if k /= d && k /= n then
                        -- Somewhere in the interior of the search region and edit graph.
                        let prev = getK (pred k)
                            next = getK (succ k) in
                        if x prev < x next then
                          moveDownFrom next
                        else
                          moveRightFrom prev
                      else
                        -- The upper/right extent of the search region or edit graph, whichever is smaller.
                        moveRightFrom (getK (pred k))
                  put (v Array.// [(Diagonal k, (x', script))])
                  return $! if x' >= n && (x' - k) >= m then
                    Just (script, d)
                  else
                    Nothing
                  where -- | Move downward from a given vertex, inserting the element for the corresponding row.
                        moveDownFrom (Endpoint x script) = Endpoint x (if (x - k) < m then That (bs ! (x - k)) : script else script)

                        -- | Move rightward from a given vertex, deleting the element for the corresponding column.
                        moveRightFrom (Endpoint x script) = Endpoint (succ x) (if x < n then This (as ! x) : script else script)

                        -- | Slide down any diagonal edges from a given vertex.
                        slideFrom (Endpoint x script)
                          | x >= 0, x < n
                          , (x - k) >= 0, (x - k) < m
                          , a <- as ! x
                          , b <- bs ! (x - k)
                          , a `eq` b  = slideFrom (Endpoint (succ x) (These a b : script))
                          | otherwise =            Endpoint       x               script


-- Implementation details

-- | The state stored by Myers’ algorithm; an array of m + n + 1 values indicating the maximum x-index reached and path taken along each diagonal.
type MyersState a b = Array.Array Diagonal (Int, EditScript a b)

-- | Compute the empty state of length m + n + 1 for a given edit graph.
emptyStateForGraph :: EditGraph a b -> MyersState a b
emptyStateForGraph (EditGraph as bs) = let (n, m) = (length as, length bs) in
  Array.listArray (Diagonal (negate m), Diagonal n) (repeat (0, []))

-- | Evaluate some function for each value in a list until one returns a value or the list is exhausted.
for :: [a] -> (a -> Myers c d (Maybe b)) -> Myers c d (Maybe b)
for all run = foldr (\ a b -> (<|>) <$> run a <*> b) (return Nothing) all
{-# INLINE for #-}


-- | Lifted showing of arrays.
liftShowsVector :: Show i => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> Array.Array i a -> ShowS
liftShowsVector sp sl d = liftShowsPrec sp sl d . toList

-- | Lifted showing of These.
liftShowsThese :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> These a b -> ShowS
liftShowsThese sa sb d t = case t of
  This a -> showsUnaryWith sa "This" d a
  That b -> showsUnaryWith sb "That" d b
  These a b -> showsBinaryWith sa sb "These" d a b

-- | Lifted showing of edit scripts.
liftShowsEditScript :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> EditScript a b -> ShowS
liftShowsEditScript sa sb _ = showListWith (liftShowsThese sa sb 0)

-- | Lifted showing of edit graph endpoints.
liftShowsEndpoint :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> Int -> Endpoint a b -> ShowS
liftShowsEndpoint sp1 sp2 d (Endpoint x script) = showsBinaryWith showsPrec (liftShowsEditScript sp1 sp2) "Endpoint" d x script


-- Instances

instance Show2 EditGraph where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (EditGraph as bs) = showsBinaryWith (liftShowsVector sp1 sl1) (liftShowsVector sp2 sl2) "EditGraph" d as bs

instance Show2 Endpoint where
  liftShowsPrec2 sp1 _ sp2 _ = liftShowsEndpoint sp1 sp2
