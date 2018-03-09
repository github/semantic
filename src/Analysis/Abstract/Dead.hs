{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Dead where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Data.Semigroup.Reducer as Reducer
import Data.Set (delete)
import Prologue

-- | The effects necessary for dead code analysis.
type DeadCode term = State (Dead term)


-- | An analysis tracking dead (unreachable) code.
newtype DeadCodeAnalysis m term value (effects :: [* -> *]) a = DeadCodeAnalysis { runDeadCodeAnalysis :: m term value effects a }
  deriving (Applicative, Functor, Effectful, Monad, MonadFail)

deriving instance MonadEvaluator term value effects m => MonadEvaluator term value effects (DeadCodeAnalysis m)

-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (Effectful (m term value), Member (State (Dead term)) effects) => Dead term -> DeadCodeAnalysis m term value effects ()
killAll = lift . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Effectful (m term value), Member (State (Dead term)) effects) => Ord term => term -> DeadCodeAnalysis m term value effects ()
revive t = lift (modify (Dead . delete t . unDead))

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


instance ( Corecursive term
         , Effectful (m term value)
         , Foldable (Base term)
         , Member (State (Dead term)) effects
         , MonadAnalysis term value effects m
         , MonadEvaluator term value effects m
         , Ord term
         , Recursive term
         )
         => MonadAnalysis term value effects (DeadCodeAnalysis m) where
  type RequiredEffects term value (DeadCodeAnalysis m) = State (Dead term) ': RequiredEffects term value m
  analyzeTerm term = do
    revive (embedSubterm term)
    liftAnalyze analyzeTerm term

  evaluateModule term = do
    killAll (subterms term)
    liftEvaluate evaluateModule term
