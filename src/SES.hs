module SES (ses) where

import Patch
import Diff
import Control.Monad.Free

ses :: Functor f => Eq a => [a] -> [a] -> [Free f (Patch a)]
ses [] b = (Pure . Insert) <$> b
ses a [] = (Pure . Delete) <$> a
ses (a : as) (b : bs) = case recur a b of
  Just f -> f : ses as bs
  Nothing -> if SES.cost delete < SES.cost insert then delete else insert where
    delete = (Pure . Delete $ a) : ses as (b : bs)
    insert = (Pure . Insert $ b) : ses (a : as) bs

cost :: Functor f => [Free f (Patch a)] -> Integer
cost = _

recur :: Functor f => Eq a => a -> a -> Maybe (Free f (Patch a))
recur a b = _
