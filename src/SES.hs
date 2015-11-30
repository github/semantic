module SES where

import Patch
import Diff
import Term
import Control.Monad.Free
import Control.Monad.State
import Data.Foldable (minimumBy)
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

diffAt :: Compare String Info -> Cost String Info -> (Integer, Integer) -> [Term String Info] -> [Term String Info] -> State (Map.Map (Integer, Integer) [(Diff String Info, Integer)]) [Diff String Info]
diffAt _ _ _ [] [] = return []
diffAt _ _ _ [] bs = return $ (Pure . Insert) <$> bs
diffAt _ _ _ as [] = return $ (Pure . Delete) <$> as
diffAt diffTerms cost (i, j) (a : as) (b : bs) = do
  cachedDiffs <- get
  case Map.lookup (i, j) cachedDiffs of
    Just diffs -> return $ fmap fst diffs
    Nothing -> do
      put $ Map.insert (i, j) [] cachedDiffs
      return []
  where
    delete = (id &&& cost) (Pure . Delete $ a)
    insert = (id &&& cost) (Pure . Insert $ b)
    copy diff = (id &&& cost) diff
    sumCost script = sum $ snd <$> script
    best options = minimumBy (comparing sumCost) options
    recur = diffAt diffTerms cost
