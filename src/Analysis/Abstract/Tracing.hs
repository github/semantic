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
import Prologue

-- | An effect to trace visited 'Configuration's.
type Tracer trace term value = Writer (trace (Configuration (LocationFor value) term value))

-- | The effects necessary for tracing analyses.
type TracingEffects trace term value = Tracer trace term value ': EvaluatorEffects term value

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
evaluateTrace :: forall trace value term
              . ( Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , Monoid (trace (Configuration (LocationFor value) term value))
                , Ord (Cell (LocationFor value) value)
                , Ord term
                , Ord value
                , Pointed trace
                , Recursive term
                , MonadAddressable (LocationFor value) value (TracingAnalysis trace term value)
                , MonadValue term value (TracingAnalysis trace term value)
                , Semigroup (Cell (LocationFor value) value)
                )
              => term
              -> Final (TracingEffects trace term value) value
evaluateTrace = run @(TracingEffects trace term value) . runEvaluator . runTracingAnalysis . evaluateTerm


newtype TracingAnalysis trace term value a
  = TracingAnalysis { runTracingAnalysis :: Evaluator term value (TracingEffects trace term value) a }
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term value (TracingAnalysis trace term value)

instance ( Corecursive term
         , Evaluatable (Base term)
         , FreeVariables term
         , MonadAddressable (LocationFor value) value (TracingAnalysis trace term value)
         , MonadValue term value (TracingAnalysis trace term value)
         , Pointed trace
         , Recursive term
         , Semigroup (Cell (LocationFor value) value)
         )
         => MonadAnalysis term value (TracingAnalysis trace term value) where
  analyzeTerm term = getConfiguration (embedSubterm term) >>= trace . point >> eval term

trace :: trace (Configuration (LocationFor value) term value) -> TracingAnalysis trace term value ()
trace w = TracingAnalysis (Evaluator (tell w))
