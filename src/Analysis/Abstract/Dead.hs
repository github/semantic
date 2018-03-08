{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Dead where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Data.Set (delete)
import Prologue

-- | The effects necessary for dead code analysis.
type DeadCodeEffects term value = State (Dead term) ': EvaluatorEffects term value


-- | Run a dead code analysis of the given program.
evaluateDead :: forall m term value effects
             .  ( term ~ TermFor m
                , value ~ ValueFor m
                , effects ~ Effects m
                , Corecursive term
                , Member (State (Dead term)) effects
                , Effectful m
                , Evaluatable (Base term)
                , Foldable (Base term)
                , FreeVariables term
                , MonadAnalysis m
                , Ord (LocationFor value)
                , Ord term
                , Recursive term
                , RunEffects effects value
                , Semigroup (CellFor value)
                )
             => term
             -> DeadCodeAnalysis m value
evaluateDead term = do
  killAll (subterms term)
  evaluateTerm term


-- | An analysis tracking dead (unreachable) code.
newtype DeadCodeAnalysis m a = DeadCodeAnalysis { runDeadCodeAnalysis :: m a }
  deriving (Applicative, Functor, Effectful, Monad, MonadEvaluator, MonadFail)


-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (Effectful m, Member (State (Dead (TermFor m))) (Effects m)) => Dead (TermFor m) -> DeadCodeAnalysis m ()
killAll = lift . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Effectful m, Member (State (Dead (TermFor m))) (Effects m)) => Ord (TermFor m) => (TermFor m) -> DeadCodeAnalysis m ()
revive t = lift (modify (Dead . delete t . unDead))

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


instance ( Corecursive (TermFor m)
         , Effectful m
         , Foldable (Base (TermFor m))
         , Member (State (Dead (TermFor m))) (Effects m)
         , MonadAnalysis m
         , MonadEvaluator m
         , Ord (TermFor m)
         , Recursive (TermFor m)
         )
         => MonadAnalysis (DeadCodeAnalysis m) where
  analyzeTerm term = do
    revive (embedSubterm term)
    liftAnalyze analyzeTerm term

  evaluateModule term = do
    killAll (subterms term)
    liftEvaluate evaluateModule term
