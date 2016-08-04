{-# LANGUAGE RankNTypes #-}
module Interpreter (Comparable, DiffConstructor, diffTerms) where

import Algorithm
import Category
import Data.Align.Generic
import Data.Functor.Foldable
import Data.Functor.Both
import Data.Hashable
import Data.RandomWalkSimilarity
import Data.Record
import Data.These
import Diff
import qualified Control.Monad.Free.Church as F
import Info
import Patch
import Prologue hiding (lookup)
import SES
import Syntax
import Term

-- | Returns whether two terms are comparable
type Comparable leaf annotation = Term leaf annotation -> Term leaf annotation -> Bool

-- | Constructs a diff from the CofreeF containing its annotation and syntax. This function has the opportunity to, for example, cache properties in the annotation.
type DiffConstructor leaf annotation = CofreeF (Syntax leaf) (Both annotation) (Diff leaf annotation) -> Diff leaf annotation

-- | Diff two terms, given a function that determines whether two terms can be compared and a cost function.
diffTerms :: (Eq leaf, Hashable leaf, Eq (Record fields), HasField fields Category) => DiffConstructor leaf (Record fields) -> Comparable leaf (Record fields) -> SES.Cost (Diff leaf (Record fields)) -> Term leaf (Record fields) -> Term leaf (Record fields) -> Diff leaf (Record fields)
diffTerms construct comparable cost a b
  | not (comparable a b) = replacing a b
  | otherwise = fromMaybe (replacing a b) $ run construct comparable cost (algorithmWithTerms a b)

-- | Constructs an algorithm and runs it
constructAndRun :: (Eq leaf, Hashable leaf, Eq (Record fields), HasField fields Category) => DiffConstructor leaf (Record fields) -> Comparable leaf (Record fields) -> SES.Cost (Diff leaf (Record fields)) -> Term leaf (Record fields) -> Term leaf (Record fields) -> Maybe (Diff leaf (Record fields))
constructAndRun construct comparable cost t1 t2
  | not $ comparable t1 t2 = Nothing
  | (category <$> t1) == (category <$> t2) = hylo construct runCofree <$> zipTerms t1 t2
  | otherwise =
  run construct comparable cost $ algorithm a b where
    algorithm (Indexed a') (Indexed b') = do
      diffs <- byIndex a' b'
      annotate (Indexed diffs)
    algorithm (Leaf a') (Leaf b') | a' == b' = annotate $ Leaf b'
    algorithm _ _ = recursively t1 t2
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = pure . construct . (both annotation1 annotation2 :<)

algorithmWithTerms :: Eq leaf => Term leaf (Record fields) -> Term leaf (Record fields) -> Algorithm (Term leaf (Record fields)) (Diff leaf (Record fields)) (Diff leaf (Record fields))
algorithmWithTerms t1 t2 = case (unwrap t1, unwrap t2) of
  (Indexed a, Indexed b) -> do
    diffs <- byIndex a b
    annotate (Indexed diffs)
  (Leaf a, Leaf b) | a == b -> annotate (Leaf b)
  _ -> recursively t1 t2
  where annotate = pure . wrap . (both (extract t1) (extract t2) :<)

-- | Runs the diff algorithm
run :: (Eq leaf, Hashable leaf, Eq (Record fields), HasField fields Category) => DiffConstructor leaf (Record fields) -> Comparable leaf (Record fields) -> SES.Cost (Diff leaf (Record fields)) -> Algorithm (Term leaf (Record fields)) (Diff leaf (Record fields)) (Diff leaf (Record fields)) -> Maybe (Diff leaf (Record fields))
run construct comparable cost = runAlgorithm construct recur cost getLabel . fmap Just
  where recur = constructAndRun construct comparable cost
        getLabel (h :< t) = (category h, case t of
          Leaf s -> Just s
          _ -> Nothing)

runAlgorithm :: (Functor f, GAlign f, Eq a, Eq annotation, Eq (f (Cofree f annotation)), Prologue.Foldable f, Traversable f, Hashable label) =>
  (CofreeF f (Both annotation) (Free (CofreeF f (Both annotation)) (Patch (Cofree f annotation))) -> Free (CofreeF f (Both annotation)) (Patch (Cofree f annotation))) ->
  (Cofree f annotation -> Cofree f annotation -> Maybe (Free (CofreeF f (Both annotation)) (Patch (Cofree f annotation)))) ->
  SES.Cost (Free (CofreeF f (Both annotation)) (Patch (Cofree f annotation))) ->
  (forall b. CofreeF f annotation b -> label) ->
  Algorithm (Cofree f annotation) (Free (CofreeF f (Both annotation)) (Patch (Cofree f annotation))) a ->
  a
runAlgorithm construct recur cost getLabel = F.iter $ \case
  Recursive a b f -> f (maybe (replacing a b) (construct . (both (extract a) (extract b) :<)) $ do
    aligned <- galign (unwrap a) (unwrap b)
    traverse (these (Just . deleting) (Just . inserting) recur) aligned)
  ByIndex as bs f -> f (ses recur cost as bs)
  ByRandomWalkSimilarity as bs f -> f (rws recur getLabel as bs)
