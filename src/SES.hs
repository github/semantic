module SES where

import Prologue
import Patch
import Diff
import Term
import qualified Data.Map as Map

-- | A function that maybe creates a diff from two terms.
type Compare a annotation = Term a annotation -> Term a annotation -> Maybe (Diff a annotation)

-- | A function that computes the cost of a diff.
type Cost a annotation = Diff a annotation -> Rational

-- | Find the shortest edit script (diff) between two terms given a function to compute the cost.
ses :: Compare a annotation -> Cost a annotation -> [Term a annotation] -> [Term a annotation] -> [Diff a annotation]
ses diffTerms cost as bs = fst <$> evalState diffState Map.empty where
  diffState = diffAt diffTerms cost (0, 0) as bs

-- | Find the shortest edit script between two terms at a given vertex in the edit graph.
diffAt :: Compare a annotation -> Cost a annotation -> (Integer, Integer) -> [Term a annotation] -> [Term a annotation] -> State (Map.Map (Integer, Integer) [(Diff a annotation, Rational)]) [(Diff a annotation, Rational)]
diffAt _ _ _ [] [] = pure []
diffAt _ cost _ [] bs = pure $ foldr toInsertions [] bs where
  toInsertions each = consWithCost cost (free . Pure . Insert $ each)
diffAt _ cost _ as [] = pure $ foldr toDeletions [] as where
  toDeletions each = consWithCost cost (free . Pure . Delete $ each)
diffAt diffTerms cost (i, j) (a : as) (b : bs) = do
  cachedDiffs <- get
  case Map.lookup (i, j) cachedDiffs of
    Just diffs -> pure diffs
    Nothing -> do
      down <- recur (i, succ j) as (b : bs)
      right <- recur (succ i, j) (a : as) bs
      nomination <- fmap best $ case diffTerms a b of
        Just diff -> do
          diagonal <- recur (succ i, succ j) as bs
          pure [ delete down, insert right, consWithCost cost diff diagonal ]
        Nothing -> pure [ delete down, insert right ]
      cachedDiffs' <- get
      put $ Map.insert (i, j) nomination cachedDiffs'
      pure nomination
  where
    delete = consWithCost cost (free . Pure . Delete $ a)
    insert = consWithCost cost (free . Pure . Insert $ b)
    costOf [] = 0
    costOf ((_, c) : _) = c
    best = minimumBy (comparing costOf)
    recur = diffAt diffTerms cost

-- | Prepend a diff to the list with the cumulative cost.
consWithCost :: Cost a annotation -> Diff a annotation -> [(Diff a annotation, Rational)] -> [(Diff a annotation, Rational)]
consWithCost cost diff rest = (diff, cost diff + maybe 0 snd (fst <$> uncons rest)) : rest
