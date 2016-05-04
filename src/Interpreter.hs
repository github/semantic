module Interpreter (interpret, Comparable, diffTerms) where

import Algorithm
import Category
import Data.Functor.Foldable
import Control.Comonad
import Control.Monad.Trans.Free
import Control.Comonad.Trans.Cofree
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
interpret comparable cost a b = fromMaybe (free . Pure $ Replace a b) $ constructAndRun comparable cost a b

-- | Constructs an algorithm and runs it
constructAndRun :: (Eq a, Eq annotation) => Comparable a annotation -> Cost a annotation -> Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
constructAndRun _ _ a b | a == b = hylo (\termF -> free . Free $ headF termF :< tailF termF) runCofree <$> zipTerms a b

constructAndRun comparable _ a b | not $ comparable a b = Nothing

constructAndRun comparable cost t1 t2 =
  run comparable cost $ algorithm a b where
    algorithm (Indexed a') (Indexed b') = free . Free $ ByIndex a' b' (annotate . Indexed)
    algorithm (Keyed a') (Keyed b') = free . Free $ ByKey a' b' (annotate . Keyed)
    algorithm (Leaf a') (Leaf b') | a' == b' = annotate $ Leaf b'
    algorithm a' b' = free . Free $ Recursive (cofree (annotation1 :< a')) (cofree (annotation2 :< b')) (free . Pure)
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = free . Pure . free . Free . Diff.annotate (both annotation1 annotation2)

-- | Runs the diff algorithm
run :: (Eq a, Eq annotation) => Comparable a annotation -> Cost a annotation -> Algorithm a annotation (Diff a annotation) -> Maybe (Diff a annotation)
run comparable cost algorithm = case runFree algorithm of
  (Pure diff) -> Just diff
  (Free (Recursive t1 t2 f)) -> run comparable cost . f $ recur a b where
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = free . Free . (Both (annotation1, annotation2) :<)

    recur (Indexed a') (Indexed b') | length a' == length b' = annotate . Indexed $ zipWith (interpret comparable cost) a' b'
    recur (Fixed a') (Fixed b') | length a' == length b' = annotate . Fixed $ zipWith (interpret comparable cost) a' b'
    recur (Keyed a') (Keyed b') | Map.keys a' == bKeys = annotate . Keyed . Map.fromList . fmap repack $ bKeys where
      bKeys = Map.keys b'
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key x y = interpret comparable cost (x ! key) (y ! key)
    recur _ _ = free . Pure $ Replace (cofree (annotation1 :< a)) (cofree (annotation2 :< b))
  Free (ByKey a b f) -> run comparable cost $ f byKey where
    byKey = Map.fromList $ toKeyValue <$> List.union aKeys bKeys
    toKeyValue key | key `List.elem` deleted = (key, free . Pure . Delete $ a ! key)
    toKeyValue key | key `List.elem` inserted = (key, free . Pure . Insert $ b ! key)
    toKeyValue key = (key, interpret comparable cost (a ! key) (b ! key))
    aKeys = Map.keys a
    bKeys = Map.keys b
    deleted = aKeys \\ bKeys
    inserted = bKeys \\ aKeys
  Free (ByIndex a b f) -> run comparable cost . f $ ses (constructAndRun comparable cost) cost a b

