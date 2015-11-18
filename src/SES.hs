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
  Just f -> f : ses recur as bs
  Nothing -> if SES.cost delete < SES.cost insert then delete else insert where
    delete = (Pure . Delete $ a) : ses recur as (b : bs)
    insert = (Pure . Insert $ b) : ses recur (a : as) bs

cost :: [Diff a] -> Integer
cost as = sum $ Diff.cost <$> as
