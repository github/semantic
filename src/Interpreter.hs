module Interpreter (interpret, Comparable, diffTerms) where

import Algorithm
import Category
import Control.Arrow
import Control.Comonad.Cofree
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

-- | Diff two terms, given the default Categorizable.comparable function.
diffTerms :: (Eq a, Eq annotation, Categorizable annotation) => Term a annotation -> Term a annotation -> Diff a annotation
diffTerms = interpret comparable

-- | Diff two terms, given a function that determines whether two terms can be compared.
interpret :: (Eq a, Eq annotation) => Comparable a annotation -> Term a annotation -> Term a annotation -> Diff a annotation
interpret comparable a b = fromMaybe (Pure $ Replace a b) $ constructAndRun comparable a b

-- | Constructs an algorithm and runs it
constructAndRun :: (Eq a, Eq annotation) => Comparable a annotation -> Term a annotation -> Term a annotation -> Maybe (Diff a annotation)
constructAndRun _ a b | a == b = hylo (curry $ Free . uncurry Annotated) (copoint &&& unwrap) <$> zipTerms a b where

constructAndRun comparable a b | not $ comparable a b = Nothing

constructAndRun comparable (annotation1 :< a) (annotation2 :< b) =
  run comparable $ algorithm a b where
    algorithm (Indexed a') (Indexed b') = Free $ ByIndex a' b' (annotate . Indexed)
    algorithm (Keyed a') (Keyed b') = Free $ ByKey a' b' (annotate . Keyed)
    algorithm (Leaf a') (Leaf b') | a' == b' = annotate $ Leaf b'
    algorithm a' b' = Free $ Recursive (annotation1 :< a') (annotation2 :< b') Pure
    annotate = Pure . Free . Annotated (both annotation1 annotation2)

-- | Runs the diff algorithm
run :: (Eq a, Eq annotation) => Comparable a annotation -> Algorithm a annotation (Diff a annotation) -> Maybe (Diff a annotation)
run _ (Pure diff) = Just diff

run comparable (Free (Recursive (annotation1 :< a) (annotation2 :< b) f)) = run comparable . f $ recur a b where
  recur (Indexed a') (Indexed b') | length a' == length b' = annotate . Indexed $ zipWith (interpret comparable) a' b'
  recur (Fixed a') (Fixed b') | length a' == length b' = annotate . Fixed $ zipWith (interpret comparable) a' b'
  recur (Keyed a') (Keyed b') | Map.keys a' == bKeys = annotate . Keyed . Map.fromList . fmap repack $ bKeys
    where
      bKeys = Map.keys b'
      repack key = (key, interpretInBoth key a' b')
      interpretInBoth key x y = interpret comparable (x ! key) (y ! key)
  recur _ _ = Pure $ Replace (annotation1 :< a) (annotation2 :< b)

  annotate = Free . Annotated (both annotation1 annotation2)

run comparable (Free (ByKey a b f)) = run comparable $ f byKey where
  byKey = Map.fromList $ toKeyValue <$> List.union aKeys bKeys
  toKeyValue key | List.elem key deleted = (key, Pure . Delete $ a ! key)
  toKeyValue key | List.elem key inserted = (key, Pure . Insert $ b ! key)
  toKeyValue key = (key, interpret comparable (a ! key) (b ! key))
  aKeys = Map.keys a
  bKeys = Map.keys b
  deleted = aKeys \\ bKeys
  inserted = bKeys \\ aKeys

run comparable (Free (ByIndex a b f)) = run comparable . f $ ses (constructAndRun comparable) diffCost a b
