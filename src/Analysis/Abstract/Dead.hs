{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Dead where

import Analysis.Abstract.Evaluating
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
evaluateDead :: forall term value effects m
             .  ( m ~ Evaluation term value effects
                , effects ~ DeadCodeEffects term value
                , Corecursive term
                , Evaluatable (Base term)
                , Foldable (Base term)
                , FreeVariables term
                , MonadAddressable (LocationFor value) m
                , MonadValue value m
                , Ord (LocationFor value)
                , Ord term
                , Recursive term
                , Semigroup (CellFor value)
                )
             => term
             -> Final (DeadCodeEffects term value) value
evaluateDead term = run @(DeadCodeEffects term value) . lower @(DeadCodeAnalysis m) $ do
  killAll (subterms term)
  evaluateTerm term


-- | An analysis tracking dead (unreachable) code.
newtype DeadCodeAnalysis m a = DeadCodeAnalysis { runDeadCodeAnalysis :: m a }
  deriving (Applicative, Functor, LiftEffect, Monad, MonadEvaluator, MonadFail)


-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (LiftEffect m, Member (State (Dead (TermFor m))) (Effects m)) => Dead (TermFor m) -> DeadCodeAnalysis m ()
killAll = lift . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (LiftEffect m, Member (State (Dead (TermFor m))) (Effects m)) => Ord (TermFor m) => (TermFor m) -> DeadCodeAnalysis m ()
revive t = lift (modify (Dead . delete t . unDead))

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


instance ( Corecursive (TermFor m)
         , LiftEffect m
         , Member (State (Dead (TermFor m))) (Effects m)
         , MonadAnalysis m
         , MonadEvaluator m
         , Ord (TermFor m)
         , Recursive (TermFor m)
         )
         => MonadAnalysis (DeadCodeAnalysis m) where
  analyzeTerm term = do
    revive (embedSubterm term)
    DeadCodeAnalysis (analyzeTerm (second runDeadCodeAnalysis <$> term))
