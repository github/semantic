{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Control.Abstract.Analysis
import Control.Abstract.Evaluator
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Prologue

type Trace trace term value = trace (ConfigurationFor term value)
type TraceFor trace m = Trace trace (TermFor m) (ValueFor m)
type Tracer trace term value = Writer (Trace trace term value)
type TracerFor trace m = Writer (TraceFor trace m)
-- | The effects necessary for tracing analyses.
type TracingEffects trace term value = Tracer trace term value ': EvaluatorEffects term value

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
newtype TracingAnalysis (trace :: * -> *) m a
  = TracingAnalysis { runTracingAnalysis :: m a }
  deriving (Applicative, Functor, Generic1, LiftEffect, Monad, MonadEvaluator, MonadFail)

instance ( Corecursive (TermFor m)
         , LiftEffect m
         , Member (TracerFor trace m) (Effects m)
         , MonadAnalysis m
         , MonadEvaluator m
         , Ord (LocationFor (ValueFor m))
         , Recursive (TermFor m)
         , Reducer (ConfigurationFor (TermFor m) (ValueFor m)) (TraceFor trace m)
         )
         => MonadAnalysis (TracingAnalysis trace m) where
  analyzeTerm term = do
    config <- getConfiguration (embedSubterm term)
    trace (Reducer.unit config)
    delegateAnalyzeTerm term

type instance TermFor  (TracingAnalysis trace m) = TermFor  m
type instance ValueFor (TracingAnalysis trace m) = ValueFor m

trace :: ( LiftEffect m
         , Member (TracerFor trace m) (Effects m)
         )
      => TraceFor trace m
      -> TracingAnalysis trace m ()
trace = lift . tell
