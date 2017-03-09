{-# LANGUAGE Strict #-}
module SES where

import qualified Data.Map as Map
import Data.These
import Prologue


-- | Edit constructor for two terms, if comparable. Otherwise returns Nothing.
type Comparable term = term -> term -> Bool

-- | A function that computes the cost of an edit.
type Cost term = These term term -> Int

-- | Find the shortest edit script (diff) between two terms given a function to compute the cost.
ses :: Comparable term -> Cost term -> [term] -> [term] -> [These term term]
ses canCompare cost as bs = fst <$> evalState diffState Map.empty where
  diffState = diffAt canCompare cost (0, 0) as bs

-- | Find the shortest edit script between two terms at a given vertex in the edit graph.
diffAt :: Comparable term -> Cost term -> (Int, Int) -> [term] -> [term] -> State (Map.Map Int [(These term term, Int)]) [(These term term, Int)]
diffAt canCompare cost (i, j) as bs
  | (a : as) <- as, (b : bs) <- bs = do
  cachedDiffs <- get
  case Map.lookup cantorKey cachedDiffs of
    Just diffs -> pure diffs
    Nothing -> do
      down <- recur (i, succ j) as (b : bs)
      right <- recur (succ i, j) (a : as) bs
      nomination <- best <$> if canCompare a b
        then do
          diagonal <- recur (succ i, succ j) as bs
          pure [ delete a down, insert b right, consWithCost cost (These a b) diagonal ]
        else pure [ delete a down, insert b right ]
      cachedDiffs' <- get
      put $ Map.insert cantorKey nomination cachedDiffs'
      pure nomination
  | null as = pure $ foldr insert [] bs
  | null bs = pure $ foldr delete [] as
  | otherwise = pure []
  where
    cantorKey = ((i + j) * (i + j + 1)) `div` 2 + j
    delete = consWithCost cost . This
    insert = consWithCost cost . That
    costOf [] = 0
    costOf ((_, c) : _) = c
    best = minimumBy (comparing costOf)
    recur = diffAt canCompare cost

-- | Prepend an edit script and the cumulative cost onto the edit script.
consWithCost :: Cost term -> These term term -> [(These term term, Int)] -> [(These term term, Int)]
consWithCost cost edit rest = (edit, cost edit + maybe 0 snd (fst <$> uncons rest)) : rest
