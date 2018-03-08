{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Control.Abstract.Analysis
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Prologue

type Trace trace term value = trace (ConfigurationFor term value)
type TraceFor trace m = Trace trace (TermFor m) (ValueFor m)
type Tracer trace term value = Writer (Trace trace term value)
type TracerFor trace m = Writer (TraceFor trace m)

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
newtype TracingAnalysis (trace :: * -> *) m (effects :: [* -> *]) a
  = TracingAnalysis { runTracingAnalysis :: m effects a }
  deriving (Applicative, Functor, Effectful, Monad, MonadEvaluator, MonadFail)

instance ( Corecursive (TermFor (m effects))
         , Effectful m
         , Member (TracerFor trace (m effects)) effects
         , MonadAnalysis (m effects)
         , MonadEvaluator (m effects)
         , Ord (LocationFor (ValueFor (m effects)))
         , Recursive (TermFor (m effects))
         , Reducer (ConfigurationFor (TermFor (m effects)) (ValueFor (m effects))) (TraceFor trace (m effects))
         )
         => MonadAnalysis (TracingAnalysis trace m effects) where
  analyzeTerm term = do
    config <- getConfiguration (embedSubterm term)
    trace (Reducer.unit config)
    liftAnalyze analyzeTerm term

trace :: ( Effectful m
         , Member (TracerFor trace (m effects)) effects
         )
      => TraceFor trace (m effects)
      -> TracingAnalysis trace m effects ()
trace = lift . tell
