module SES where

import Patch
import Diff
import Term
import Control.Monad.Free
import Control.Monad.State
import Data.Foldable (minimumBy)
import Data.List (uncons)
import qualified Data.Map as Map
import Data.Ord (comparing)

type Compare a annotation = Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
type Cost a annotation = Diff a annotation -> Integer

ses :: Compare a annotation -> Cost a annotation -> [Term a annotation] -> [Term a annotation] -> [Diff a annotation]
ses diffTerms cost as bs = fst <$> evalState diffState Map.empty where
  diffState = diffAt diffTerms cost (0, 0) as bs

diffAt :: Compare a annotation -> Cost a annotation -> (Integer, Integer) -> [Term a annotation] -> [Term a annotation] -> State (Map.Map (Integer, Integer) [(Diff a annotation, Integer)]) [(Diff a annotation, Integer)]
diffAt _ _ _ [] [] = return []
diffAt _ cost _ [] bs = return $ foldr toInsertions [] bs where
  toInsertions each = consWithCost cost (Pure . Insert $ each)
diffAt _ cost _ as [] = return $ foldr toDeletions [] as where
  toDeletions each = consWithCost cost (Pure . Delete $ each)
diffAt diffTerms cost (i, j) (a : as) (b : bs) = do
  cachedDiffs <- get
  case Map.lookup (i, j) cachedDiffs of
    Just diffs -> return diffs
    Nothing -> do
      down <- recur (i, succ j) as (b : bs)
      right <- recur (succ i, j) (a : as) bs
      nomination <- fmap best $ case diffTerms a b of
        Just diff -> do
          diagonal <- recur (succ i, succ j) as bs
          return [ delete down, insert right, consWithCost cost diff diagonal ]
        Nothing -> return [ delete down, insert right ]
      cachedDiffs' <- get
      put $ Map.insert (i, j) nomination cachedDiffs'
      return nomination
  where
    delete = consWithCost cost (Pure . Delete $ a)
    insert = consWithCost cost (Pure . Insert $ b)
    costOf [] = 0
    costOf ((_, c) : _) = c
    best = minimumBy (comparing costOf)
    recur = diffAt diffTerms cost

consWithCost :: Cost a annotation -> Diff a annotation -> [(Diff a annotation, Integer)] -> [(Diff a annotation, Integer)]
consWithCost cost diff rest = (diff, cost diff + maybe 0 snd (fst <$> uncons rest)) : rest
