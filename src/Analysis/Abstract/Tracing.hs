{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing
( Tracing
) where

import Control.Abstract.Analysis
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Semigroup.Reducer as Reducer
import Data.Union
import Prologue

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
newtype Tracing (trace :: * -> *) m (effects :: [* -> *]) a = Tracing (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m effects)                    => MonadControl term (Tracing trace m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (Tracing trace m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (Tracing trace m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (Tracing trace m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (Tracing trace m effects)

instance ( Corecursive term
         , Effectful m
         , Member (Writer (trace (Configuration location term value))) effects
         , MonadAnalysis location term value (m effects)
         , Ord location
         , Reducer (Configuration location term value) (trace (Configuration location term value))
         )
      => MonadAnalysis location term value (Tracing trace m effects) where
  type Effects location term value (Tracing trace m effects) = Writer (trace (Configuration location term value)) ': Effects location term value (m effects)

  analyzeTerm recur term = do
    config <- getConfiguration (embedSubterm term)
    raise (tell @(trace (Configuration location term value)) (Reducer.unit config))
    liftAnalyze analyzeTerm recur term

  analyzeModule = liftAnalyze analyzeModule
