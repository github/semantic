{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Control.Abstract.Evaluator
import Control.Abstract.Value
import Control.Monad.Effect.Writer
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Evaluatable
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Prologue

-- | The effects necessary for tracing analyses.
type TracingEffects trace term value = Writer trace ': EvaluatorEffects term value

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
evaluateTrace :: forall trace value term
              . ( Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , Monoid trace
                , Ord (Cell (LocationFor value) value)
                , Ord term
                , Ord value
                , Recursive term
                , Reducer (Configuration (LocationFor value) term value) trace
                , MonadAddressable (LocationFor value) value (TracingAnalysis trace term value (TracingEffects trace term value))
                , MonadValue term value (TracingAnalysis trace term value (TracingEffects trace term value))
                , Semigroup (Cell (LocationFor value) value)
                )
              => term
              -> Final (TracingEffects trace term value) value
evaluateTrace = run @(TracingEffects trace term value) . runEvaluator . runTracingAnalysis @trace . evaluateTerm


newtype TracingAnalysis trace term value effects a
  = TracingAnalysis { runTracingAnalysis :: Evaluator term value effects a }
  deriving (Applicative, Functor, LiftEffect, Monad)

deriving instance Member Fail effects                           => MonadFail (TracingAnalysis trace term value effects)
deriving instance Members (EvaluatorEffects term value) effects => MonadEvaluator term value (TracingAnalysis trace term value effects)

instance ( Corecursive term
         , Evaluatable (Base term)
         , FreeVariables term
         , Member (Writer trace) effects
         , MonadAddressable (LocationFor value) value (TracingAnalysis trace term value effects)
         , MonadValue term value (TracingAnalysis trace term value effects)
         , Recursive term
         , Reducer (Configuration (LocationFor value) term value) trace
         , Semigroup (Cell (LocationFor value) value)
         )
         => MonadAnalysis term value (TracingAnalysis trace term value effects) where
  analyzeTerm term = getConfiguration (embedSubterm term) >>= trace . Reducer.unit >> eval term

trace :: Member (Writer trace) effects
      => trace
      -> TracingAnalysis trace term value effects ()
trace w = lift (tell w)
