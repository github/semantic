module SES where

import Patch
import Diff
import Control.Monad.Free

ses :: Functor f => Eq a => [a] -> [a] -> [Free f (Patch a)]
ses a b | Prelude.null a = (Pure . Insert) <$> b
ses a b | Prelude.null b = (Pure . Delete) <$> a
