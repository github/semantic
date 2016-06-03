module Interpreter (Comparable, DiffConstructor, diffTerms) where

import Algorithm
import Data.Functor.Foldable
import Data.Functor.Both
import qualified Data.OrderedMap as Map
import qualified Data.List as List
import Data.List ((\\))
import Data.OrderedMap ((!))
import Diff
import Operation
import Patch
import Prologue hiding (lookup)
import SES
import Syntax
import Term

-- | Returns whether two terms are comparable
type Comparable a annotation = Term a annotation -> Term a annotation -> Bool

-- | Constructs a diff from the CofreeF containing its annotation and syntax. This function has the opportunity to, for example, cache properties in the annotation.
type DiffConstructor leaf annotation = CofreeF (Syntax leaf) (Both annotation) (Diff leaf annotation) -> Diff leaf annotation

-- | Diff two terms, given a function that determines whether two terms can be compared and a cost function.
diffTerms :: (Eq a, Eq annotation) => DiffConstructor a annotation -> Comparable a annotation -> Cost a annotation -> Term a annotation -> Term a annotation -> Diff a annotation
diffTerms construct comparable cost a b = fromMaybe (pure $ Replace a b) $ constructAndRun construct comparable cost a b

-- | Constructs an algorithm and runs it
constructAndRun :: (Eq a, Eq annotation) => DiffConstructor a annotation -> Comparable a annotation -> Cost a annotation -> Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
constructAndRun construct _ _ a b | a == b = hylo construct runCofree <$> zipTerms a b

constructAndRun _ comparable _ a b | not $ comparable a b = Nothing

constructAndRun construct comparable cost t1 t2 =
  run construct comparable cost $ algorithm a b where
    algorithm (Indexed a') (Indexed b') = free . Free $ ByIndex a' b' (annotate . Indexed)
    algorithm (Keyed a') (Keyed b') = free . Free $ ByKey a' b' (annotate . Keyed)
    algorithm (Leaf a') (Leaf b') | a' == b' = annotate $ Leaf b'
    algorithm a' b' = free . Free $ Recursive (cofree (annotation1 :< a')) (cofree (annotation2 :< b')) pure
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = pure . construct . (both annotation1 annotation2 :<)

-- | Runs the diff algorithm
run :: (Eq a, Eq annotation) => DiffConstructor a annotation -> Comparable a annotation -> Cost a annotation -> Algorithm a annotation (Diff a annotation) -> Maybe (Diff a annotation)
run construct comparable cost algorithm = case runFree algorithm of
  Pure diff -> Just diff
  Free (Recursive t1 t2 f) -> run construct comparable cost . f $ recur a b where
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = construct . (both annotation1 annotation2 :<)

    recur (Indexed a') (Indexed b') | length a' == length b' = annotate . Indexed $ zipWith (diffTerms construct comparable cost) a' b'
    recur (Fixed a') (Fixed b') | length a' == length b' = annotate . Fixed $ zipWith (diffTerms construct comparable cost) a' b'
    recur (Keyed a') (Keyed b') | Map.keys a' == bKeys = annotate . Keyed . Map.fromList . fmap repack $ bKeys where
      bKeys = Map.keys b'
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key x y = diffTerms construct comparable cost (x ! key) (y ! key)
    recur _ _ = pure $ Replace (cofree (annotation1 :< a)) (cofree (annotation2 :< b))

  Free (ByKey a b f) -> run construct comparable cost $ f byKey where
    byKey = Map.fromList $ toKeyValue <$> List.union aKeys bKeys
    toKeyValue key | key `List.elem` deleted = (key, pure . Delete $ a ! key)
    toKeyValue key | key `List.elem` inserted = (key, pure . Insert $ b ! key)
    toKeyValue key = (key, diffTerms construct comparable cost (a ! key) (b ! key))
    aKeys = Map.keys a
    bKeys = Map.keys b
    deleted = aKeys \\ bKeys
    inserted = bKeys \\ aKeys

  Free (ByIndex a b f) -> run construct comparable cost . f $ ses (constructAndRun construct comparable cost) cost a b
