{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Analysis.Abstract.Evaluating
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

type Trace trace term value = trace (ConfigurationFor term value)
type TraceFor trace m = Trace trace (AnalysisTerm m) (AnalysisValue m)
type Tracer trace term value = Writer (Trace trace term value)
-- | The effects necessary for tracing analyses.
type TracingEffects trace term value = Tracer trace term value ': EvaluatorEffects term value

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
evaluateTrace :: forall trace value term
              . ( Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , Monoid (Trace trace term value)
                , Ord (CellFor value)
                , Ord term
                , Ord value
                , Recursive term
                , Reducer (ConfigurationFor term value) (Trace trace term value)
                , MonadAddressable (LocationFor value) (TracingAnalysis trace (Evaluation term value) (TracingEffects trace term value))
                , MonadAnalysis (Evaluation term value (TracingEffects trace term value))
                , MonadValue value (TracingAnalysis trace (Evaluation term value) (TracingEffects trace term value))
                , Semigroup (CellFor value)
                )
              => term
              -> Final (TracingEffects trace term value) value
evaluateTrace = run @(TracingEffects trace term value) . runEvaluator . runEvaluation . runTracingAnalysis @trace . evaluateTerm


newtype TracingAnalysis (trace :: * -> *) underlying (effects :: [* -> *]) a
  = TracingAnalysis { runTracingAnalysis :: underlying effects a }
  deriving (Applicative, Functor, LiftEffect, Monad, MonadFail)

deriving instance (AnalysisTerm (underlying effects) ~ term, AnalysisValue (underlying effects) ~ value, MonadEvaluator (underlying effects)) => MonadEvaluator (TracingAnalysis trace underlying effects)

instance ( Corecursive (AnalysisTerm (underlying effects))
         , Evaluatable (Base (AnalysisTerm (underlying effects)))
         , FreeVariables (AnalysisTerm (underlying effects))
         , LiftEffect underlying
         , Member (Tracer trace (AnalysisTerm (underlying effects)) (AnalysisValue (underlying effects))) effects
         , MonadAddressable (LocationFor (AnalysisValue (underlying effects))) (TracingAnalysis trace underlying effects)
         , MonadAnalysis (underlying effects)
         , MonadValue (AnalysisValue (underlying effects)) (TracingAnalysis trace underlying effects)
         , Recursive (AnalysisTerm (underlying effects))
         , Reducer (ConfigurationFor (AnalysisTerm (underlying effects)) (AnalysisValue (underlying effects))) (TraceFor trace (underlying effects))
         , Semigroup (CellFor (AnalysisValue (underlying effects)))
         )
         => MonadAnalysis (TracingAnalysis trace underlying effects) where
  analyzeTerm term = getConfiguration (embedSubterm term) >>= trace . Reducer.unit >> TracingAnalysis (analyzeTerm (second runTracingAnalysis <$> term))

type instance AnalysisTerm  (TracingAnalysis trace underlying effects) = AnalysisTerm  (underlying effects)
type instance AnalysisValue (TracingAnalysis trace underlying effects) = AnalysisValue (underlying effects)

trace :: ( LiftEffect underlying
         , Member (Tracer trace (AnalysisTerm (underlying effects)) (AnalysisValue (underlying effects))) effects
         )
      => TraceFor trace (underlying effects)
      -> TracingAnalysis trace underlying effects ()
trace w = lift (tell w)
