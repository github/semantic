module Interpreter (interpret, Comparable) where

import Prelude hiding (lookup)
import Algorithm
import Diff
import Operation
import Patch
import SES
import Syntax
import Term
import Control.Monad.Free
import Control.Comonad.Cofree hiding (unwrap)
import qualified OrderedMap as Map
import OrderedMap ((!))
import Data.Maybe

hylo :: Functor f => (t -> f b -> b) -> (a -> (t, f a)) -> a -> b
hylo down up a = down annotation $ hylo down up <$> syntax where
  (annotation, syntax) = up a

-- | Constructs an algorithm and runs it
constructAndRun :: (Eq a, Eq annotation) => Comparable a annotation -> Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
constructAndRun _ a b | a == b = hylo introduce eliminate <$> zipTerms a b where
  eliminate :: Cofree f a -> (a, f (Cofree f a))
  eliminate (extract :< unwrap) = (extract, unwrap)
  introduce :: (annotation, annotation) -> Syntax a (Diff a annotation) -> Diff a annotation
  introduce ann syntax = Free $ Annotated ann syntax
constructAndRun comparable a b | not $ comparable a b = Nothing
constructAndRun comparable (annotation1 :< a) (annotation2 :< b) =
  run comparable $ algorithm a b where
    algorithm (Indexed a') (Indexed b') = Free $ ByIndex a' b' (annotate . Indexed)
    algorithm (Keyed a') (Keyed b') = Free $ ByKey a' b' (annotate . Keyed)
    algorithm (Leaf a') (Leaf b') | a' == b' = annotate $ Leaf b'
    algorithm a' b' = Free $ Recursive (annotation1 :< a') (annotation2 :< b') Pure
    annotate = Pure . Free . Annotated (annotation1, annotation2)

-- | Runs the diff algorithm
run :: (Eq a, Eq annotation) => Comparable a annotation -> Algorithm a annotation (Diff a annotation) -> Maybe (Diff a annotation)
run _ (Pure diff) = Just diff

run comparable (Free (Recursive (annotation1 :< a) (annotation2 :< b) f)) = run comparable . f $ recur a b where
  recur (Indexed a') (Indexed b') | length a' == length b' = annotate . Indexed $ zipWith (interpret comparable) a' b'
  recur (Fixed a') (Fixed b') | length a' == length b' = annotate . Fixed $ zipWith (interpret comparable) a' b'
  recur (Keyed a') (Keyed b') | Map.keys a' == Map.keys b' = annotate . Keyed . Map.fromList . fmap repack $ Map.keys b'
    where
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key x y = interpret comparable (x ! key) (y ! key)
  recur _ _ = Pure $ Replace (annotation1 :< a) (annotation2 :< b)

  annotate = Free . Annotated (annotation1, annotation2)

run comparable (Free (ByKey a b f)) = run comparable $ f byKey where
  byKey = Map.unions [ deleted, inserted, patched ]
  deleted = (Pure . Delete) <$> Map.difference a b
  inserted = (Pure . Insert) <$> Map.difference b a
  patched = Map.intersectionWith (interpret comparable) a b

run comparable (Free (ByIndex a b f)) = run comparable . f $ ses (constructAndRun comparable) diffCost a b

type Comparable a annotation = Term a annotation -> Term a annotation -> Bool

interpret :: (Eq a, Eq annotation) => Comparable a annotation -> Term a annotation -> Term a annotation -> Diff a annotation
interpret comparable a b = fromMaybe (Pure $ Replace a b) $ constructAndRun comparable a b
