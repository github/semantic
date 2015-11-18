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

constructAndRun :: Eq a => Term a Info -> Term a Info -> Maybe (Diff a)
constructAndRun a b =
  run $ algorithm a b where
    algorithm (_ :< Indexed a) (_ :< Indexed b) = Free $ ByIndex a b (Pure . Free . Indexed)
    algorithm (_ :< Keyed a) (_ :< Keyed b) = Free $ ByKey a b (Pure . Free . Keyed)
    algorithm (_ :< Leaf a) (_ :< Leaf b) | a == b = Pure . Free $ Leaf b
    algorithm a b = Free $ Recursive a b Pure

run :: Eq a => Algorithm a (Diff a) -> Maybe (Diff a)
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

run (Free (ByIndex a b f)) = run . f $ ses constructAndRun cost a b

interpret :: Eq a => Term a Info -> Term a Info -> Diff a
interpret a b = maybeReplace $ constructAndRun a b where
  maybeReplace (Just a) = a
  maybeReplace Nothing = Pure $ Replace a b
