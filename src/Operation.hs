module Operation where

import Diff
import Data.Map
import Term

data Operation a f
  = Recursive (Term a Info) (Term a Info) (Diff a -> f)
  | ByKey (Map String (Term a Info)) (Map String (Term a Info)) (Map String (Diff a) -> f)
  | ByIndex [Term a Info] [Term a Info] ([Diff a] -> f)
  deriving Functor
