{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, KindSignatures, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Control.Abstract.Analysis
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Prologue

type Trace trace term value = trace (ConfigurationFor term value)
type Tracer trace term value = Writer (Trace trace term value)

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
newtype TracingAnalysis (trace :: * -> *) m term value (effects :: [* -> *]) a
  = TracingAnalysis { runTracingAnalysis :: m term value effects a }
  deriving (Applicative, Functor, Effectful, Monad, MonadFail)

deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (TracingAnalysis trace m term value effects)

instance ( Corecursive term
         , Effectful (m term value)
         , Member (Tracer trace term value) effects
         , MonadAnalysis term value (m term value effects)
         , MonadEvaluator term value (m term value effects)
         , Ord (LocationFor value)
         , Recursive term
         , Reducer (ConfigurationFor term value) (Trace trace term value)
         )
         => MonadAnalysis term value (TracingAnalysis trace m term value effects) where
  type RequiredEffects term value (TracingAnalysis trace m term value effects) = Writer (Trace trace term value) ': RequiredEffects term value (m term value effects)
  analyzeTerm term = do
    config <- getConfiguration (embedSubterm term)
    trace (Reducer.unit config)
    liftAnalyze analyzeTerm term

trace :: ( Effectful (m term value)
         , Member (Tracer trace term value) effects
         )
      => Trace trace term value
      -> TracingAnalysis trace m term value effects ()
trace = lift . tell
