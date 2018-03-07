{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Control.Abstract.Evaluator
import Control.Abstract.Value
import Control.Monad.Effect.Writer
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
                , Ord (CellFor value)
                , Ord term
                , Ord value
                , Recursive term
                , Reducer (ConfigurationFor term value) trace
                , MonadAddressable (LocationFor value) value (TracingAnalysis trace Evaluator term value (TracingEffects trace term value))
                , MonadAnalysis term value (Evaluator term value (TracingEffects trace term value))
                , MonadValue term value (TracingAnalysis trace Evaluator term value (TracingEffects trace term value))
                , Semigroup (CellFor value)
                )
              => term
              -> Final (TracingEffects trace term value) value
evaluateTrace = run @(TracingEffects trace term value) . runEvaluator . runTracingAnalysis @trace . evaluateTerm


newtype TracingAnalysis trace underlying term value (effects :: [* -> *]) a
  = TracingAnalysis { runTracingAnalysis :: underlying term value effects a }
  deriving (Applicative, Functor, LiftEffect, Monad, MonadFail)

deriving instance MonadEvaluator term value (underlying term value effects) => MonadEvaluator term value (TracingAnalysis trace underlying term value effects)

instance ( Corecursive term
         , Evaluatable (Base term)
         , FreeVariables term
         , LiftEffect (underlying term value)
         , Member (Writer trace) effects
         , MonadAddressable (LocationFor value) value (TracingAnalysis trace underlying term value effects)
         , MonadAnalysis term value (underlying term value effects)
         , MonadValue term value (TracingAnalysis trace underlying term value effects)
         , Recursive term
         , Reducer (ConfigurationFor term value) trace
         , Semigroup (CellFor value)
         )
         => MonadAnalysis term value (TracingAnalysis trace underlying term value effects) where
  analyzeTerm term = getConfiguration (embedSubterm term) >>= trace . Reducer.unit >> analyzeTerm term

trace :: (LiftEffect (underlying term value), Member (Writer trace) effects)
      => trace
      -> TracingAnalysis trace underlying term value effects ()
trace w = lift (tell w)
