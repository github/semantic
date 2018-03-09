{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Dead where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Data.Semigroup.Reducer as Reducer
import Data.Set (delete)
import Prologue

-- | The effects necessary for dead code analysis.
type DeadCode term = State (Dead term)


-- | An analysis tracking dead (unreachable) code.
newtype DeadCodeAnalysis m a = DeadCodeAnalysis { runDeadCodeAnalysis :: m a }
  deriving (Applicative, Functor, Effectful, Monad, MonadEvaluator, MonadFail)


-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (Effectful m, Member (State (Dead (TermFor m))) (EffectsFor m)) => Dead (TermFor m) -> DeadCodeAnalysis m ()
killAll = lift . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Effectful m, Member (State (Dead (TermFor m))) (EffectsFor m)) => Ord (TermFor m) => (TermFor m) -> DeadCodeAnalysis m ()
revive t = lift (modify (Dead . delete t . unDead))

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


instance ( Corecursive (TermFor m)
         , Effectful m
         , Foldable (Base (TermFor m))
         , Member (State (Dead (TermFor m))) (EffectsFor m)
         , MonadAnalysis m
         , MonadEvaluator m
         , Ord (TermFor m)
         , Recursive (TermFor m)
         )
         => MonadAnalysis (DeadCodeAnalysis m) where
  type EffectsRequiredFor (DeadCodeAnalysis m) = DeadCode (TermFor m) ': EffectsRequiredFor m
  analyzeTerm term = do
    revive (embedSubterm term)
    liftAnalyze analyzeTerm term

  evaluateModule term = do
    killAll (subterms term)
    liftEvaluate evaluateModule term
