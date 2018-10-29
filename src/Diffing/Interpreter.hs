{-# LANGUAGE LambdaCase, TypeOperators, UndecidableInstances #-}
module Diffing.Interpreter
( diffTerms
, diffTermPair
, stripDiff
) where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.Sum
import qualified Data.Diff as Diff
import Data.Term
import Diffing.Algorithm
import Diffing.Algorithm.RWS
import Prologue

-- | Diff two à la carte terms recursively.
diffTerms :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax)
          => Term syntax ann
          -> Term syntax ann
          -> Diff.Diff syntax ann ann
diffTerms t1 t2 = stripDiff (fromMaybe (Diff.replacing t1' t2') (run (runNonDet (runDiff (algorithmForTerms t1' t2')))))
  where (t1', t2') = ( defaultFeatureVectorDecorator t1
                     , defaultFeatureVectorDecorator t2)

-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff :: Functor syntax
          => Diff.Diff syntax (FeatureVector, ann) (FeatureVector, ann)
          -> Diff.Diff syntax ann ann
stripDiff = bimap snd snd

-- | Diff a 'These' of terms.
diffTermPair :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => These (Term syntax ann) (Term syntax ann) -> Diff.Diff syntax ann ann
diffTermPair = these Diff.deleting Diff.inserting diffTerms


-- | Run an 'Algorithm' to completion in an 'Alternative' context using the supplied comparability & equivalence relations.
runDiff :: (Alternative m, Carrier sig m, Diffable syntax, Eq1 syntax, Member NonDet sig, Monad m, Traversable syntax)
        => Algorithm
             (Term syntax (FeatureVector, ann))
             (Term syntax (FeatureVector, ann))
             (Diff.Diff syntax (FeatureVector, ann) (FeatureVector, ann))
             (DiffC (Term syntax (FeatureVector, ann)) (Term syntax (FeatureVector, ann)) (Diff.Diff syntax (FeatureVector, ann) (FeatureVector, ann)) m)
             result
        -> m result
runDiff = runDiffC . interpret . runAlgorithm


newtype DiffC term1 term2 diff m a = DiffC { runDiffC :: m a }

instance ( Alternative m
         , Carrier sig m
         , Diffable syntax
         , Eq1 syntax
         , Member NonDet sig
         , Monad m
         , Traversable syntax
         )
      => Carrier
        (Diff (Term syntax (FeatureVector, ann)) (Term syntax (FeatureVector, ann)) (Diff.Diff syntax (FeatureVector, ann) (FeatureVector, ann)) :+: sig)
        (DiffC (Term syntax (FeatureVector, ann)) (Term syntax (FeatureVector, ann)) (Diff.Diff syntax (FeatureVector, ann) (FeatureVector, ann)) m) where
  ret = DiffC . ret
  eff = DiffC . handleSum (eff . handleCoercible) (\case
    Diff t1 t2 k -> runDiff (algorithmForTerms t1 t2) <|> pure (Diff.replacing t1 t2) >>= runDiffC . k
    Linear (Term (In ann1 f1)) (Term (In ann2 f2)) k -> Diff.merge (ann1, ann2) <$> tryAlignWith (runDiff . diffThese) f1 f2 >>= runDiffC . k
    RWS as bs k -> traverse (runDiff . diffThese) (rws comparableTerms equivalentTerms as bs) >>= runDiffC . k
    Delete a k -> runDiffC (k (Diff.deleting a))
    Insert b k -> runDiffC (k (Diff.inserting b))
    Replace a b k -> runDiffC (k (Diff.replacing a b)))
