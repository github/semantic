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
type DeadCodeEvaluating t v
  = '[ State (Dead t)                  -- The set of dead terms
     , Fail                            -- Failure with an error message
     , State (Store (LocationFor v) v) -- The heap
     , State (EnvironmentFor v)        -- Global (imperative) environment
     , Reader (EnvironmentFor v)       -- Local environment (e.g. binding over a closure)
     , Reader (Linker t)               -- Cache of unevaluated modules
     , State (Linker v)                -- Cache of evaluated modules
     ]


evaluateDead :: forall term value
             .  ( AbstractValue value
                , Corecursive term
                , Evaluatable (Base term)
                , Foldable (Base term)
                , FreeVariables term
                , MonadAddressable (LocationFor value) value (DeadCodeEvaluation term value)
                , MonadFunction term value (DeadCodeEvaluation term value)
                , Ord (LocationFor value)
                , Ord term
                , Recursive term
                , Semigroup (Cell (LocationFor value) value)
                )
             => term
             -> Final (DeadCodeEvaluating term value) value
evaluateDead term = run @(DeadCodeEvaluating term value) . runEvaluator . runDeadCodeEvaluation $ do
  killAll (subterms term)
  evaluateTerm term
  where subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Dead a
        subterms term = para (foldMap (uncurry ((<>) . point))) term <> point term


newtype DeadCodeEvaluation term value a = DeadCodeEvaluation { runDeadCodeEvaluation :: Evaluator (DeadCodeEvaluating term value) term value a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term value (DeadCodeEvaluation term value)


-- | A set of “dead” (unreachable) terms.
newtype Dead a = Dead { unDead :: Set a }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Pointed, Show)

-- | Update the current 'Dead' set.
killAll :: Dead t -> DeadCodeEvaluation t v ()
killAll = DeadCodeEvaluation . Evaluator . put

-- | Revive a single term, removing it from the current 'Dead' set.
revive :: Ord t => t -> DeadCodeEvaluation t v ()
revive t = DeadCodeEvaluation (Evaluator (modify (Dead . delete t . unDead)))


instance ( AbstractValue v
         , Corecursive t
         , Evaluatable (Base t)
         , FreeVariables t
         , MonadAddressable (LocationFor v) v (DeadCodeEvaluation t v)
         , MonadFunction t v (DeadCodeEvaluation t v)
         , Ord t
         , Recursive t
         , Semigroup (Cell (LocationFor v) v)
         )
         => MonadAnalysis t v (DeadCodeEvaluation t v) where
  evaluateTerm = foldSubterms (\ term -> do
    revive (embed (subterm <$> term))
    eval term)
