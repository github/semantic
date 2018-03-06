{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Control.Abstract.Evaluator
import Control.Abstract.Value
import Control.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Control.Monad.Effect.Writer
import Data.Abstract.Address
import Data.Abstract.Configuration
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Value
import Prologue

-- | The effects necessary for tracing analyses.
type TracingEffects trace term value
  = '[ Writer (trace (Configuration (LocationFor value) term value)) -- For 'MonadTrace'.
     , Fail                                           -- For 'MonadFail'.
     , State (StoreFor value)                -- For 'MonadStore'.
     , Reader (EnvironmentFor value)         -- For 'MonadEnv'.
     , State (EnvironmentFor value)         -- For 'MonadEnv'.
     , Reader (Linker term)
     , State (Linker value)
     ]

-- | Linear trace analysis.
evaluateTrace :: forall value term
              . ( Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , Ord (Cell (LocationFor value) value)
                , Ord term
                , Ord value
                , Recursive term
                , MonadAddressable (LocationFor value) value (TracingAnalysis [] term value)
                , MonadValue term value (TracingAnalysis [] term value)
                , Semigroup (Cell (LocationFor value) value)
                )
              => term
              -> Final (TracingEffects [] term value) value
evaluateTrace = run @(TracingEffects [] term value) . runEvaluator . runTracingAnalysis . evaluateTerm

-- | Reachable configuration analysis.
evaluateReach :: forall value term
              . ( Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , Ord (Cell (LocationFor value) value)
                , Ord (LocationFor value)
                , Ord term
                , Ord value
                , Recursive term
                , MonadAddressable (LocationFor value) value (TracingAnalysis Set term value)
                , MonadValue term value (TracingAnalysis Set term value)
                , Semigroup (Cell (LocationFor value) value)
                )
              => term
              -> Final (TracingEffects Set term value) value
evaluateReach = run @(TracingEffects Set term value) . runEvaluator . runTracingAnalysis . evaluateTerm


newtype TracingAnalysis trace term value a = TracingAnalysis { runTracingAnalysis :: Evaluator (TracingEffects trace term value) term value a }
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
