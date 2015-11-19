module Interpreter (interpret, Comparable) where

import Algorithm
import Control.Monad.Free
import Control.Comonad.Cofree
import Operation
import Diff
import Syntax
import Data.Map
import Patch
import SES
import Term

constructAndRun :: (Eq a, Eq annotation) => Comparable a annotation -> Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
constructAndRun comparable a b | not $ comparable a b = Nothing
constructAndRun comparable a b =
  run comparable $ algorithm a b where
    algorithm (annotation1 :< Indexed a) (annotation2 :< Indexed b) = Free $ ByIndex a b (Pure . Free . Annotated (annotation1, annotation2) . Indexed)
    algorithm (annotation1 :< Keyed a) (annotation2 :< Keyed b) = Free $ ByKey a b (Pure . Free . Annotated (annotation1, annotation2) . Keyed)
    algorithm (annotation1 :< Leaf a) (annotation2 :< Leaf b) | a == b = Pure . Free . Annotated (annotation1, annotation2) $ Leaf b
    algorithm a b = Free $ Recursive a b Pure

run :: (Eq a, Eq annotation) => Comparable a annotation -> Algorithm a annotation (Diff a annotation) -> Maybe (Diff a annotation)
run _ (Pure diff) = Just diff

run comparable (Free (Recursive a b f)) = run comparable . f $ recur a b where
  recur (annotation1 :< Indexed a') (annotation2 :< Indexed b') | length a' == length b' =
    Free . Annotated (annotation1, annotation2) . Indexed $ zipWith (interpret comparable) a' b'
  recur (annotation1 :< Fixed a') (annotation2 :< Fixed b') | length a' == length b' =
    Free . Annotated (annotation1, annotation2) . Fixed $ zipWith (interpret comparable) a' b'
  recur (annotation1 :< Keyed a') (annotation2 :< Keyed b') | keys a' == keys b' =
    Free . Annotated (annotation1, annotation2) . Keyed . fromList . fmap repack $ keys b' where
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key a' b' = maybeInterpret (Data.Map.lookup key a') (Data.Map.lookup key b')
      maybeInterpret (Just a) (Just b) = interpret comparable a b
  recur _ _ = Pure $ Replace a b

run comparable (Free (ByKey a b f)) = run comparable $ f byKey where
  byKey = unions [ deleted, inserted, patched ]
  deleted = (Pure . Delete) <$> difference a b
  inserted = (Pure . Insert) <$> difference b a
  patched = intersectionWith (interpret comparable) a b

run comparable (Free (ByIndex a b f)) = run comparable . f $ ses (constructAndRun comparable) cost a b

type Comparable a annotation = Term a annotation -> Term a annotation -> Bool

interpret :: (Eq a, Eq annotation) => Comparable a annotation -> Term a annotation -> Term a annotation -> Diff a annotation
interpret comparable a b = maybeReplace $ constructAndRun comparable a b where
  maybeReplace (Just a) = a
  maybeReplace Nothing = Pure $ Replace a b
