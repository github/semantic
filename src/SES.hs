module SES where

import Patch
import Control.Monad.Free

ses a b | Prelude.null a = (Pure . Insert) <$> b
ses a b | Prelude.null b = (Pure . Delete) <$> a
