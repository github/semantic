{-# LANGUAGE Strict #-}
module SES where

import qualified Data.Map as Map
import Patch
import Prologue


-- | Edit constructor for two terms, if comparable. Otherwise returns Nothing.
type Compare term edit = term -> term -> Maybe edit

-- | A function that computes the cost of an edit.
type Cost edit = edit -> Int

-- | Find the shortest edit script (diff) between two terms given a function to compute the cost.
ses :: Applicative edit => Compare term (edit (Patch term)) -> Cost (edit (Patch term)) -> [term] -> [term] -> [edit (Patch term)]
ses diffTerms cost as bs = fst <$> evalState diffState Map.empty where
  diffState = diffAt diffTerms cost (0, 0) as bs

-- | Find the shortest edit script between two terms at a given vertex in the edit graph.
diffAt :: Applicative edit => Compare term (edit (Patch term)) -> Cost (edit (Patch term)) -> (Int, Int) -> [term] -> [term] -> State (Map.Map (Int, Int) [(edit (Patch term), Int)]) [(edit (Patch term), Int)]
diffAt diffTerms cost (i, j) as bs
  | (a : as) <- as, (b : bs) <- bs = do
  cachedDiffs <- get
  case Map.lookup (i, j) cachedDiffs of
    Just diffs -> pure diffs
    Nothing -> do
      down <- recur (i, succ j) as (b : bs)
      right <- recur (succ i, j) (a : as) bs
      nomination <- best <$> case diffTerms a b of
        Just diff -> do
          diagonal <- recur (succ i, succ j) as bs
          pure [ delete a down, insert b right, consWithCost cost diff diagonal ]
        Nothing -> pure [ delete a down, insert b right ]
      cachedDiffs' <- get
      put $ Map.insert (i, j) nomination cachedDiffs'
      pure nomination
  | null as = pure $ foldr insert [] bs
  | null bs = pure $ foldr delete [] as
  | otherwise = pure []
  where
    delete = consWithCost cost . deleting
    insert = consWithCost cost . inserting
    costOf [] = 0
    costOf ((_, c) : _) = c
    best = minimumBy (comparing costOf)
    recur = diffAt diffTerms cost

-- | Prepend an edit script and the cumulative cost onto the edit script.
consWithCost :: Cost edit -> edit -> [(edit, Int)] -> [(edit, Int)]
consWithCost cost edit rest = (edit, cost edit + maybe 0 snd (fst <$> uncons rest)) : rest
