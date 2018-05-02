{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.Tracing
( Tracing
) where

import Control.Abstract.Analysis
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Live
import Data.Semigroup.Reducer as Reducer
import Prologue

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
newtype Tracing (trace :: * -> *) m (effects :: [* -> *]) a = Tracing { runTracing :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (Tracing trace m)
deriving instance Evaluator location term value m => Evaluator location term value (Tracing trace m)

instance ( Corecursive term
         , Effectful m
         , Member (Reader (Live location value)) effects
         , Member (Writer (trace (Configuration location term value))) effects
         , MonadAnalysis location term value effects m
         , Ord location
         , Reducer (Configuration location term value) (trace (Configuration location term value))
         )
      => MonadAnalysis location term value effects (Tracing trace m) where
  analyzeTerm recur term = do
    config <- getConfiguration (embedSubterm term)
    raise (tell @(trace (Configuration location term value)) (Reducer.unit config))
    liftAnalyze analyzeTerm recur term

  analyzeModule = liftAnalyze analyzeModule

instance ( Interpreter m effects
         , MonadEvaluator location term value effects m
         , Monoid (trace (Configuration location term value))
         )
      => Interpreter (Tracing trace m) (Writer (trace (Configuration location term value)) ': effects) where
  type Result (Tracing trace m) (Writer (trace (Configuration location term value)) ': effects) result = Result m effects (result, trace (Configuration location term value))
  interpret = interpret . runTracing . raiseHandler runWriter
