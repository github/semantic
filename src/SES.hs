module SES where

import Patch
import Diff
import Control.Monad.Free
import Control.Comonad.Cofree
import Term

type Compare a annotation = Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
type Cost a annotation = Diff a annotation -> Integer

ses :: Compare a annotation -> Cost a annotation -> [Term a annotation] -> [Term a annotation] -> [Diff a annotation]
ses _ _ [] b = (Pure . Insert) <$> b
ses _ _ a [] = (Pure . Delete) <$> a
ses recur cost (a : as) (b : bs) = case recur a b of
  Just f | deleteCost < insertCost && deleteCost < copyCost -> delete
         | insertCost < copyCost -> insert
         | otherwise -> copy
    where
      copy = f : ses recur cost as bs
      copyCost = sumCost copy
  Nothing | deleteCost < insertCost -> delete
          | otherwise -> insert
  where
    delete = (Pure . Delete $ a) : ses recur cost as (b : bs)
    insert = (Pure . Insert $ b) : ses recur cost (a : as) bs
    deleteCost = sumCost delete
    insertCost = sumCost insert
    sumCost a = sum $ cost <$> a
