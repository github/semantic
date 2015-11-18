module SES where

import Patch
import Diff

ses :: Eq a => [a] -> [a] -> [Either (Patch a) (a, a)]
ses [] b = (Left . Insert) <$> b
ses a [] = (Left . Delete) <$> a
ses (a : as) (b : bs) | a == b = Right (a, b) : ses as bs
