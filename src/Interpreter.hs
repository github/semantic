module Interpreter where

import Algorithm
import Control.Monad.Free
import Control.Comonad.Cofree
import Operation
import Diff

recur :: Term a Info -> Term a Info -> Maybe (Diff a)
recur a b =
  run algorithm where
    algorithm = Free $ Recursive a b Pure


run :: Algorithm a (Diff a) -> Maybe (Diff a)
run (Pure diff) = Just diff
