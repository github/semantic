{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
  deriving (Applicative, Functor, Monad, MonadFail)

deriving instance Effectful effects (m effects) => Effectful effects (TracingAnalysis trace m effects)
deriving instance MonadEvaluator effects m => MonadEvaluator effects (TracingAnalysis trace m)

instance ( Corecursive (TermFor m)
         , Effectful effects (m effects)
         , Member (TracerFor trace m) effects
         , MonadAnalysis effects m
         , MonadEvaluator effects m
         , Ord (LocationFor (ValueFor m))
         , Recursive (TermFor m)
         , Reducer (ConfigurationFor (TermFor m) (ValueFor m)) (TraceFor trace m)
         )
         => MonadAnalysis effects (TracingAnalysis trace m) where
  analyzeTerm term = do
    config <- getConfiguration (embedSubterm term)
    trace (Reducer.unit config)
    liftAnalyze analyzeTerm term

trace :: ( Effectful effects (m effects)
         , Member (TracerFor trace m) effects
         )
      => TraceFor trace m
      -> TracingAnalysis trace m effects ()
trace = lift . tell
