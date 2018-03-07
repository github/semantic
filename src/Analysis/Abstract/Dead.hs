{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Dead where

import Control.Abstract.Addressable
import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Data.Set (delete)
import Prologue

-- | The effects necessary for dead code analysis.
type DeadCodeEffects term value = State (Dead term) ': EvaluatorEffects term value


-- | Run a dead code analysis of the given program.
evaluateDead :: forall term value
             .  ( Corecursive term
                , Evaluatable (Base term)
                , Foldable (Base term)
                , FreeVariables term
                , MonadAddressable (LocationFor value) (DeadCodeAnalysis term value)
                , MonadValue value (DeadCodeAnalysis term value)
                , Ord (LocationFor value)
                , Ord term
                , Recursive term
                , Semigroup (CellFor value)
                )
             => term
             -> Final (DeadCodeEffects term value) value
evaluateDead term = run @(DeadCodeEffects term value) . runEvaluator . runDeadCodeAnalysis $ do
  killAll (subterms term)
  evaluateTerm term
  where subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Dead a
        subterms term = term `cons` para (foldMap (uncurry cons)) term


-- | A newtype wrapping 'Evaluator' which performs a dead code analysis on evaluation.
newtype DeadCodeAnalysis term value a = DeadCodeAnalysis { runDeadCodeAnalysis :: Evaluator term value (DeadCodeEffects term value) a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance Ord (LocationFor value) => MonadEvaluator (DeadCodeAnalysis term value)


-- | A set of “dead” (unreachable) terms.
newtype Dead a = Dead { unDead :: Set a }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord a => Reducer a (Dead a)

-- | Update the current 'Dead' set.
killAll :: Dead t -> DeadCodeAnalysis t v ()
killAll = DeadCodeAnalysis . Evaluator . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: Ord t => t -> DeadCodeAnalysis t v ()
revive t = DeadCodeAnalysis (Evaluator (modify (Dead . delete t . unDead)))


instance ( Corecursive t
         , Evaluatable (Base t)
         , FreeVariables t
         , MonadAddressable (LocationFor v) (DeadCodeAnalysis t v)
         , MonadValue v (DeadCodeAnalysis t v)
         , Ord t
         , Recursive t
         , Semigroup (CellFor v)
         )
         => MonadAnalysis (DeadCodeAnalysis t v) where
  analyzeTerm term = do
    revive (embedSubterm term)
    eval term

type instance AnalysisTerm (DeadCodeAnalysis term value) = term
type instance AnalysisValue (DeadCodeAnalysis term value) = value
