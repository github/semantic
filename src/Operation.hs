module Operation where

import Diff
import OrderedMap
import qualified Data.Text as T
import Term

data Operation a annotation f
  = Recursive (Term a annotation) (Term a annotation) (Diff a annotation -> f)
  | ByKey (OrderedMap T.Text (Term a annotation)) (OrderedMap T.Text (Term a annotation)) (OrderedMap T.Text (Diff a annotation) -> f)
  | ByIndex [Term a annotation] [Term a annotation] ([Diff a annotation] -> f)
  deriving Functor
