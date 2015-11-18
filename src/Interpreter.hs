module Interpreter (interpret) where

import Algorithm
import Control.Monad.Free
import Control.Comonad.Cofree
import Operation
import Diff
import Syntax

constructAndRun :: Term a Info -> Term a Info -> Maybe (Diff a)
constructAndRun a b =
  run algorithm where
    algorithm = Free $ Recursive a b Pure

run :: Algorithm a (Diff a) -> Maybe (Diff a)
run (Pure diff) = Just diff
run (Free (Recursive a b f)) = recur a b where
  recur (_ :< Indexed a') (_ :< Indexed b') | length a' == length b' =
    run $ f $ Pure Patch { old = Just a, new = Just b }
  recur _ _ = run $ f $ Pure Patch { old = Just a, new = Just b }

interpret :: Term a Info -> Term a Info -> Diff a
interpret a b = maybeReplace $ constructAndRun a b where
  maybeReplace (Just a) = a
  maybeReplace Nothing = Just a </> Just b
