{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, UndecidableInstances #-}
module Analysis.Abstract.Dead where

import Control.Abstract.Addressable
import Control.Abstract.Evaluator
import Control.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Store
import Data.Abstract.Value
import Data.Set (delete)
import Prologue

-- | The effects necessary for dead code analysis.
type DeadCodeEffects t v
  = '[ State (Dead t)                  -- The set of dead terms
     , Fail                            -- Failure with an error message
     , State (Store (LocationFor v) v) -- The heap
     , State (EnvironmentFor v)        -- Global (imperative) environment
     , Reader (EnvironmentFor v)       -- Local environment (e.g. binding over a closure)
     , Reader (Linker t)               -- Cache of unevaluated modules
     , State (Linker v)                -- Cache of evaluated modules
     ]


-- | Run a dead code analysis of the given program.
evaluateDead :: forall term value
             .  ( Corecursive term
                , Evaluatable (Base term)
                , Foldable (Base term)
                , FreeVariables term
                , MonadAddressable (LocationFor value) value (DeadCodeAnalysis term value)
                , MonadValue term value (DeadCodeAnalysis term value)
                , Ord (LocationFor value)
                , Ord term
                , Recursive term
                , Semigroup (Cell (LocationFor value) value)
                )
             => term
             -> Final (DeadCodeEffects term value) value
evaluateDead term = run @(DeadCodeEffects term value) . runEvaluator . runDeadCodeAnalysis $ do
  killAll (subterms term)
  evaluateTerm term
  where subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Dead a
        subterms term = para (foldMap (uncurry ((<>) . point))) term <> point term


-- | A newtype wrapping 'Evaluator' which performs a dead code analysis on evaluation.
newtype DeadCodeAnalysis term value a = DeadCodeAnalysis { runDeadCodeAnalysis :: Evaluator (DeadCodeEffects term value) term value a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term value (DeadCodeAnalysis term value)


-- | A set of “dead” (unreachable) terms.
newtype Dead a = Dead { unDead :: Set a }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Pointed, Show)

-- | Update the current 'Dead' set.
killAll :: Dead t -> DeadCodeAnalysis t v ()
killAll = DeadCodeAnalysis . Evaluator . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: Ord t => t -> DeadCodeAnalysis t v ()
revive t = DeadCodeAnalysis (Evaluator (modify (Dead . delete t . unDead)))


instance ( Corecursive t
         , Evaluatable (Base t)
         , FreeVariables t
         , MonadAddressable (LocationFor v) v (DeadCodeAnalysis t v)
         , MonadValue t v (DeadCodeAnalysis t v)
         , Ord t
         , Recursive t
         , Semigroup (Cell (LocationFor v) v)
         )
         => MonadAnalysis t v (DeadCodeAnalysis t v) where
  analyzeTerm term = do
    revive (embedSubterm term)
    eval term
