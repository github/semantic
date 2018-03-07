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
type TracerFor trace m = Writer (TraceFor trace m)
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
                , MonadAddressable (LocationFor value) (TracingAnalysis trace (Evaluation term value (TracingEffects trace term value)))
                , MonadAnalysis (Evaluation term value (TracingEffects trace term value))
                , MonadValue value (TracingAnalysis trace (Evaluation term value (TracingEffects trace term value)))
                , Semigroup (CellFor value)
                )
              => term
              -> Final (TracingEffects trace term value) value
evaluateTrace = run . lower @(Evaluation term value (TracingEffects trace term value)) . evaluateTerm


newtype TracingAnalysis (trace :: * -> *) m a
  = TracingAnalysis { runTracingAnalysis :: m a }
  deriving (Applicative, Functor, LiftEffect, Monad, MonadFail)

deriving instance MonadEvaluator m => MonadEvaluator (TracingAnalysis trace m)

instance ( Corecursive (AnalysisTerm m)
         , Evaluatable (Base (AnalysisTerm m))
         , FreeVariables (AnalysisTerm m)
         , LiftEffect m
         , Member (TracerFor trace m) (Effects m)
         , MonadAddressable (LocationFor (AnalysisValue m)) (TracingAnalysis trace m)
         , MonadAnalysis m
         , MonadValue (AnalysisValue m) (TracingAnalysis trace m)
         , Recursive (AnalysisTerm m)
         , Reducer (ConfigurationFor (AnalysisTerm m) (AnalysisValue m)) (TraceFor trace m)
         , Semigroup (CellFor (AnalysisValue m))
         )
         => MonadAnalysis (TracingAnalysis trace m) where
  analyzeTerm term = getConfiguration (embedSubterm term) >>= trace . Reducer.unit >> TracingAnalysis (analyzeTerm (second runTracingAnalysis <$> term))

type instance AnalysisTerm  (TracingAnalysis trace m) = AnalysisTerm  m
type instance AnalysisValue (TracingAnalysis trace m) = AnalysisValue m

trace :: ( LiftEffect m
         , Member (TracerFor trace m) (Effects m)
         )
      => TraceFor trace m
      -> TracingAnalysis trace m ()
trace w = lift (tell w)
