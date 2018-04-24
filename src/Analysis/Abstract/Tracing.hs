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
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadEvaluator location term value effects m   => MonadEvaluator location term value effects (Tracing trace m)

instance ( Corecursive term
         , Effectful m
         , Member (Writer (trace (Configuration location term value))) effects
         , MonadAnalysis location term value effects m
         , Ord location
         , Reducer (Configuration location term value) (trace (Configuration location term value))
         )
      => MonadAnalysis location term value effects (Tracing trace m) where
  type Effects location term value (Tracing trace m) = Writer (trace (Configuration location term value)) ': Effects location term value m

  analyzeTerm recur term = do
    config <- getConfiguration (embedSubterm term)
    raise (tell @(trace (Configuration location term value)) (Reducer.unit config))
    liftAnalyze analyzeTerm recur term

  analyzeModule = liftAnalyze analyzeModule
