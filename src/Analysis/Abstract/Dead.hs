{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Dead
( DeadCode
) where

import Control.Abstract.Analysis
import Data.Abstract.Module
import Data.Semigroup.Reducer as Reducer
import Data.Set (delete)
import Prologue

-- | An analysis tracking dead (unreachable) code.
newtype DeadCode m (effects :: [* -> *]) a = DeadCode (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m effects)                    => MonadControl term (DeadCode m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (DeadCode m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (DeadCode m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (DeadCode m effects)
deriving instance MonadEvaluator term value (m effects)            => MonadEvaluator term value (DeadCode m effects)

-- | A set of “dead” (unreachable) terms.
newtype Dead term = Dead { unDead :: Set term }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)

deriving instance Ord term => Reducer term (Dead term)

-- | Update the current 'Dead' set.
killAll :: (Effectful m, Member (State (Dead term)) effects) => Dead term -> DeadCode m effects ()
killAll = raise . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: (Effectful m, Member (State (Dead term)) effects) => Ord term => term -> DeadCode m effects ()
revive t = raise (modify (Dead . delete t . unDead))

-- | Compute the set of all subterms recursively.
subterms :: (Ord term, Recursive term, Foldable (Base term)) => term -> Dead term
subterms term = term `cons` para (foldMap (uncurry cons)) term


instance ( Corecursive term
         , Effectful m
         , Foldable (Base term)
         , Member (State (Dead term)) effects
         , MonadAnalysis term value (m effects)
         , Ord term
         , Recursive term
         )
         => MonadAnalysis term value (DeadCode m effects) where
  type Effects term value (DeadCode m effects) = State (Dead term) ': Effects term value (m effects)

  analyzeTerm recur term = do
    revive (embedSubterm term)
    liftAnalyze analyzeTerm recur term

  analyzeModule recur m = do
    killAll (subterms (subterm (moduleBody m)))
    liftAnalyze analyzeModule recur m
