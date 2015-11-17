module Operation where

import Diff

data Operation a f
  = Recur (Term a Info) (Term a Info) (Diff a -> f)
  | ByKey [(String, Term a Info)] [(String, Term a Info)] ([(String, Diff a)] -> f)
  | ByIndex [Term a Info] [Term a Info] ([Diff a] -> f)
