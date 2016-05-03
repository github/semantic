module Interpreter (interpret, Comparable, diffTerms) where

import Algorithm
import Category
import Control.Arrow
import Control.Comonad.Trans.Cofree
import Data.Functor.Foldable
import Control.Monad.Free
import Data.Copointed
import Data.Functor.Both
import qualified Data.OrderedMap as Map
import qualified Data.List as List
import Data.List ((\\))
import Data.Maybe
import Data.OrderedMap ((!))
import Diff
import Operation
import Patch
import Prelude hiding (lookup)
import SES
import Syntax
import Term

-- | Returns whether two terms are comparable
type Comparable a annotation = Term a annotation -> Term a annotation -> Bool

-- | Diff two terms, given the default Categorizable.comparable function and a function computing the cost of a given diff.
diffTerms :: (Eq a, Eq annotation, Categorizable annotation) => Cost a annotation -> Term a annotation -> Term a annotation -> Diff a annotation
diffTerms cost = interpret comparable cost

-- | Diff two terms, given a function that determines whether two terms can be compared.
interpret :: (Eq a, Eq annotation) => Comparable a annotation -> Cost a annotation -> Term a annotation -> Term a annotation -> Diff a annotation
interpret comparable cost a b = fromMaybe (Pure $ Replace a b) $ constructAndRun comparable cost a b

-- | Constructs an algorithm and runs it
constructAndRun :: (Eq a, Eq annotation) => Comparable a annotation -> Cost a annotation -> Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
constructAndRun _ _ a b | a == b = hylo (\termF -> Free $ Annotated (headF termF) (tailF termF)) unfix <$> zipTerms a b

constructAndRun comparable _ a b | not $ comparable a b = Nothing

constructAndRun comparable cost (Fix (annotation1 :< a)) (Fix (annotation2 :< b)) =
  run comparable cost $ algorithm a b where
    algorithm (Indexed a') (Indexed b') = Free $ ByIndex a' b' (annotate . Indexed)
    algorithm (Keyed a') (Keyed b') = Free $ ByKey a' b' (annotate . Keyed)
    algorithm (Leaf a') (Leaf b') | a' == b' = annotate $ Leaf b'
    algorithm a' b' = Free $ Recursive (Fix (annotation1 :< a')) (Fix (annotation2 :< b')) Pure
    annotate = Pure . Free . Annotated (Both (annotation1, annotation2))

-- | Runs the diff algorithm
run :: (Eq a, Eq annotation) => Comparable a annotation -> Cost a annotation -> Algorithm a annotation (Diff a annotation) -> Maybe (Diff a annotation)
run _ _ (Pure diff) = Just diff

run comparable cost (Free (Recursive (Fix (annotation1 :< a)) (Fix (annotation2 :< b)) f)) = run comparable cost . f $ recur a b where
  recur (Indexed a') (Indexed b') | length a' == length b' = annotate . Indexed $ zipWith (interpret comparable cost) a' b'
  recur (Fixed a') (Fixed b') | length a' == length b' = annotate . Fixed $ zipWith (interpret comparable cost) a' b'
  recur (Keyed a') (Keyed b') | Map.keys a' == bKeys = annotate . Keyed . Map.fromList . fmap repack $ bKeys
    where
      bKeys = Map.keys b'
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key x y = interpret comparable cost (x ! key) (y ! key)
  recur _ _ = Pure $ Replace (Fix (annotation1 :< a)) (Fix (annotation2 :< b))

  annotate = Free . Annotated (Both (annotation1, annotation2))

run comparable cost (Free (ByKey a b f)) = run comparable cost $ f byKey where
  byKey = Map.fromList $ toKeyValue <$> List.union aKeys bKeys
  toKeyValue key | List.elem key deleted = (key, Pure . Delete $ a ! key)
  toKeyValue key | List.elem key inserted = (key, Pure . Insert $ b ! key)
  toKeyValue key = (key, interpret comparable cost (a ! key) (b ! key))
  aKeys = Map.keys a
  bKeys = Map.keys b
  deleted = aKeys \\ bKeys
  inserted = bKeys \\ aKeys

run comparable cost (Free (ByIndex a b f)) = run comparable cost . f $ ses (constructAndRun comparable cost) cost a b
