module SES (ses, Compare) where

import Patch
import Diff
import Control.Monad.Free
import Control.Comonad.Cofree

type Compare a = Term a Info -> Term a Info -> Maybe (Diff a)

ses :: Compare a -> [Term a Info] -> [Term a Info] -> [Diff a]
ses _ [] b = (Pure . Insert) <$> b
ses _ a [] = (Pure . Delete) <$> a
ses recur (a : as) (b : bs) = case recur a b of
  Just f | deleteCost < insertCost && deleteCost < SES.cost copy -> delete
         | insertCost < SES.cost copy -> insert
         | otherwise -> copy
    where
      copy = f : ses recur as bs
  Nothing | deleteCost < insertCost -> delete
          | otherwise -> insert
  where
    deleteCost = SES.cost delete
    insertCost = SES.cost insert
    delete = (Pure . Delete $ a) : ses recur as (b : bs)
    insert = (Pure . Insert $ b) : ses recur (a : as) bs

cost :: [Diff a] -> Integer
cost as = sum $ Diff.cost <$> as
