module Operation where

import Diff
import OrderedMap
import Term

data Operation a annotation f
  = Recursive (Term a annotation) (Term a annotation) (Diff a annotation -> f)
  | ByKey (OrderedMap String (Term a annotation)) (OrderedMap String (Term a annotation)) (OrderedMap String (Diff a annotation) -> f)
  | ByIndex [Term a annotation] [Term a annotation] ([Diff a annotation] -> f)
  deriving Functor
