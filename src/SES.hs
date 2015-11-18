module SES (ses) where

import Patch
import Diff
import Control.Monad.Free

ses :: Functor f => Eq a => [a] -> [a] -> [Free f (Patch a)]
ses [] b = (Pure . Insert) <$> b
ses a [] = (Pure . Delete) <$> a
ses (a : as) (b : bs) = case recur a b of
  Just f -> f : ses as bs
