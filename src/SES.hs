module SES where

import Patch
import Diff

ses :: Eq a => [a] -> [a] -> [Either (Patch a) a]
ses a b | Prelude.null a = (Left . Insert) <$> b
ses a b | Prelude.null b = (Left . Delete) <$> a
