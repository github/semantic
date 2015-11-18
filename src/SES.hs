module SES (ses) where

import Patch
import Diff
import Control.Monad.Free

ses :: Eq a => [Term a Info] -> [Term a Info] -> [Diff a]
ses [] b = (Pure . Insert) <$> b
ses a [] = (Pure . Delete) <$> a
ses (a : as) (b : bs) = case recur a b of
  Just f -> f : ses as bs
  Nothing -> if SES.cost delete < SES.cost insert then delete else insert where
    delete = (Pure . Delete $ a) : ses as (b : bs)
    insert = (Pure . Insert $ b) : ses (a : as) bs

cost :: [Diff a] -> Integer
cost as = sum $ Diff.cost <$> as

recur :: Term a Info -> Term a Info -> Maybe (Diff a)
recur a b = _
