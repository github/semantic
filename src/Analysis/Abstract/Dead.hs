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
newtype DeadCodeAnalysis m (effects :: [* -> *]) a = DeadCodeAnalysis { runDeadCodeAnalysis :: m effects a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator effects m => MonadEvaluator effects (DeadCodeAnalysis m)

-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord term => Reducer term (Dead term)
deriving instance Effectful effects (m effects) => Effectful effects (DeadCodeAnalysis m effects)

-- | Update the current 'Dead' set.
killAll :: (Effectful effects (m effects), Member (State (Dead (TermFor m))) effects) => Dead (TermFor m) -> DeadCodeAnalysis m effects ()
killAll = lift . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Effectful effects (m effects), Member (State (Dead (TermFor m))) effects) => Ord (TermFor m) => (TermFor m) -> DeadCodeAnalysis m effects ()
revive t = lift (modify (Dead . delete t . unDead))

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


instance ( Corecursive (TermFor m)
         , Effectful effects (m effects)
         , Foldable (Base (TermFor m))
         , Member (State (Dead (TermFor m))) effects
         , MonadAnalysis effects m
         , MonadEvaluator effects m
         , Ord (TermFor m)
         , Recursive (TermFor m)
         )
         => MonadAnalysis effects (DeadCodeAnalysis m) where
  analyzeTerm term = do
    revive (embedSubterm term)
    liftAnalyze analyzeTerm term

  evaluateModule term = do
    killAll (subterms term)
    liftEvaluate evaluateModule term
