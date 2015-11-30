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
ses _ _ [] b = (Pure . Insert) <$> b
ses _ _ a [] = (Pure . Delete) <$> a
ses diffTerms cost (a : as) (b : bs) = case diffTerms a b of
  Just f -> minimumBy (comparing sumCost) [ delete, insert, copy f ]
  Nothing -> minimumBy (comparing sumCost) [ delete, insert ]
  where
    delete = (Pure . Delete $ a) : ses diffTerms cost as (b : bs)
    insert = (Pure . Insert $ b) : ses diffTerms cost (a : as) bs
    sumCost script = sum $ cost <$> script
    copy diff = diff : ses diffTerms cost as bs

diffAt :: Compare String Info -> Cost String Info -> (Integer, Integer) -> [Term String Info] -> [Term String Info] -> State (Map.Map (Integer, Integer) [(Diff String Info, Integer)]) [(Diff String Info, Integer)]
diffAt _ _ _ [] [] = return []
diffAt _ cost _ [] bs = return $ foldr toInsertions [] bs where
  toInsertions each rest = consWithCost cost (Pure . Insert $ each) rest
diffAt _ cost _ as [] = return $ foldr toDeletions [] as where
  toDeletions each rest = consWithCost cost (Pure . Delete $ each) rest
diffAt diffTerms cost (i, j) (a : as) (b : bs) = do
  cachedDiffs <- get
  case Map.lookup (i, j) cachedDiffs of
    Just diffs -> return diffs
    Nothing -> do
      put $ Map.insert (i, j) [] cachedDiffs
      return []
  where
    delete = consWithCost cost (Pure . Delete $ a)
    insert = consWithCost cost (Pure . Insert $ b)
    sumCost script = sum $ snd <$> script
    best options = minimumBy (comparing sumCost) options
    recur = diffAt diffTerms cost

consWithCost :: Cost a annotation -> Diff a annotation -> [(Diff a annotation, Integer)] -> [(Diff a annotation, Integer)]
consWithCost cost diff rest = (diff, cost diff + (maybe 0 snd $ fst <$> uncons rest)) : rest
