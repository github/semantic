{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s Evaluator constraint
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

deriving instance Evaluator location term value m => Evaluator location term value (Tracing trace m)
deriving instance AnalyzeModule location term value inner outer m => AnalyzeModule location term value inner outer (Tracing trace m)

instance ( Corecursive term
         , Effectful m
         , Member (Reader (Live location value)) effectsOut
         , Member (Writer (trace (Configuration location term value))) effectsOut
         , AnalyzeTerm location term value effectsIn effectsOut m
         , Evaluator location term value m
         , MonadEvaluator location term value effectsOut m
         , Ord location
         , Reducer (Configuration location term value) (trace (Configuration location term value))
         )
      => AnalyzeTerm location term value effectsIn effectsOut (Tracing trace m) where
  analyzeTerm recur term = do
    config <- getConfiguration (embedSubterm term)
    raise (tell @(trace (Configuration location term value)) (Reducer.unit config))
    Tracing (analyzeTerm (runTracing . recur) term)

instance ( Evaluator location term value m
         , Interpreter m effects
         , Monoid (trace (Configuration location term value))
         )
      => Interpreter (Tracing trace m) (Writer (trace (Configuration location term value)) ': effects) where
  type Result (Tracing trace m) (Writer (trace (Configuration location term value)) ': effects) result = Result m effects (result, trace (Configuration location term value))
  interpret = interpret . runTracing . raiseHandler runWriter
