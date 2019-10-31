{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Diffing.Interpreter
( diffTerms
, DiffTerms(..)
, stripDiff
) where

import Control.Effect.Carrier
import Control.Effect.Cull
import Control.Effect.NonDet
import qualified Data.Diff as Diff
import Data.Edit (Edit, edit)
import Data.Term
import Diffing.Algorithm
import Diffing.Algorithm.RWS
import Prologue

-- | Diff two à la carte terms recursively.
diffTerms :: (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax)
          => Term syntax ann1
          -> Term syntax ann2
          -> Diff.Diff syntax ann1 ann2
diffTerms t1 t2 = stripDiff (fromMaybe (Diff.comparing t1' t2') (run (runNonDetOnce (runDiff (algorithmForTerms t1' t2')))))
  where (t1', t2') = ( defaultFeatureVectorDecorator t1
                     , defaultFeatureVectorDecorator t2)

-- | Strips the head annotation off a diff annotated with non-empty records.
stripDiff :: Functor syntax
          => Diff.Diff syntax (FeatureVector, ann1) (FeatureVector, ann2)
          -> Diff.Diff syntax ann1 ann2
stripDiff = bimap snd snd

-- | The class of term types for which we can compute a diff.
class IsTerm term => DiffTerms term where
  -- | Diff an 'Edit' of terms.
  diffTermPair :: Edit (term ann1) (term ann2) -> Diff.Diff (Syntax term) ann1 ann2

instance (Diffable syntax, Eq1 syntax, Hashable1 syntax, Traversable syntax) => DiffTerms (Term syntax) where
  diffTermPair = edit Diff.deleting Diff.inserting diffTerms


-- | Run an 'Algorithm' to completion in an 'Alternative' context using the supplied comparability & equivalence relations.
runDiff :: Algorithm
             (Term syntax (FeatureVector, ann1))
             (Term syntax (FeatureVector, ann2))
             (Diff.Diff syntax (FeatureVector, ann1) (FeatureVector, ann2))
             (DiffC (Term syntax (FeatureVector, ann1)) (Term syntax (FeatureVector, ann2)) (Diff.Diff syntax (FeatureVector, ann1) (FeatureVector, ann2)) m)
             result
        -> m result
runDiff = runDiffC . runAlgorithm


newtype DiffC term1 term2 diff m a = DiffC { runDiffC :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadIO)

instance ( Alternative m
         , Carrier sig m
         , Diffable syntax
         , Eq1 syntax
         , Member NonDet sig
         , Monad m
         , Traversable syntax
         )
      => Carrier
        (Diff (Term syntax (FeatureVector, ann1)) (Term syntax (FeatureVector, ann2)) (Diff.Diff syntax (FeatureVector, ann1) (FeatureVector, ann2)) :+: sig)
        (DiffC (Term syntax (FeatureVector, ann1)) (Term syntax (FeatureVector, ann2)) (Diff.Diff syntax (FeatureVector, ann1) (FeatureVector, ann2)) m) where
  eff (L op) = case op of
    Diff t1 t2 k -> runDiff (algorithmForTerms t1 t2) <|> pure (Diff.comparing t1 t2) >>= k
    Linear (Term (In ann1 f1)) (Term (In ann2 f2)) k -> Diff.merge (ann1, ann2) <$> tryAlignWith (runDiff . diffEdit) f1 f2 >>= k
    RWS as bs k -> traverse (runDiff . diffEdit) (rws comparableTerms equivalentTerms as bs) >>= k
    Delete a k -> k (Diff.deleting a)
    Insert b k -> k (Diff.inserting b)
    Replace a b k -> k (Diff.comparing a b)
  eff (R other) = DiffC . eff . handleCoercible $ other
