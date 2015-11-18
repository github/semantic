module Operation where

import Diff
import Data.Map

data Operation a f
  = Recursive (Term a Info) (Term a Info) (Diff a -> f)
  | ByKey [(String, Term a Info)] [(String, Term a Info)] ([Map String (Diff a)] -> f)
  | ByIndex [Term a Info] [Term a Info] ([Diff a] -> f)
  deriving Functor
