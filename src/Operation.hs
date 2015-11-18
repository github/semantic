module Operation where

import Diff
import Data.Map
import Term

data Operation a annotation f
  = Recursive (Term a annotation) (Term a annotation) (Diff a annotation -> f)
  | ByKey (Map String (Term a annotation)) (Map String (Term a annotation)) (Map String (Diff a annotation) -> f)
  | ByIndex [Term a annotation] [Term a annotation] ([Diff a annotation] -> f)
  deriving Functor
