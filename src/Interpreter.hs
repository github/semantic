module Interpreter (interpret) where

import Algorithm
import Control.Monad.Free
import Control.Comonad.Cofree
import Operation
import Diff
import Syntax
import Data.Map
import Patch
import SES

constructAndRun :: Term a Info -> Term a Info -> Maybe (Diff a)
constructAndRun a b =
  run algorithm where
    algorithm = Free $ Recursive a b Pure

run :: Algorithm a (Diff a) -> Maybe (Diff a)
run (Pure diff) = Just diff

run (Free (Recursive a b f)) = run . f $ recur a b where
  recur (_ :< Indexed a') (_ :< Indexed b') | length a' == length b' =
    Free . Indexed $ zipWith interpret a' b'
  recur (_ :< Fixed a') (_ :< Fixed b') | length a' == length b' =
    Free . Fixed $ zipWith interpret a' b'
  recur (_ :< Keyed a') (_ :< Keyed b') | keys a' == keys b' =
    Free . Keyed . fromList . fmap repack $ keys b' where
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key a' b' = maybeInterpret (Data.Map.lookup key a') (Data.Map.lookup key b')
      maybeInterpret (Just a) (Just b) = interpret a b
  recur _ _ = Pure $ Replace a b

run (Free (ByKey a b f)) = run $ f byKey where
  byKey = unions [ deleted, inserted, patched ]
  deleted = (Pure . Delete) <$> difference a b
  inserted = (Pure . Insert) <$> difference b a
  patched = intersectionWith interpret a b

run (Free (ByIndex a b f)) = run $ f $ ses constructAndRun a b

interpret :: Term a Info -> Term a Info -> Diff a
interpret a b = maybeReplace $ constructAndRun a b where
  maybeReplace (Just a) = a
  maybeReplace Nothing = Pure $ Replace a b
