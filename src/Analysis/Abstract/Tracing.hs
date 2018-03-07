{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.Tracing where

import Analysis.Abstract.Evaluating
import Control.Abstract.Addressable
import Control.Abstract.Analysis
import Control.Abstract.Evaluator
import Control.Abstract.Value
import Control.Monad.Effect.Writer
import Data.Abstract.Configuration
import Data.Abstract.Evaluatable
import Data.Abstract.Value
import Data.Semigroup.Reducer as Reducer
import Prologue

-- | The effects necessary for tracing analyses.
type Trace trace term value = trace (ConfigurationFor term value)
type TracingEffects trace term value = Writer (Trace trace term value) ': EvaluatorEffects term value

-- | Trace analysis.
--
--   Instantiating @trace@ to @[]@ yields a linear trace analysis, while @Set@ yields a reachable state analysis.
evaluateTrace :: forall trace value term
              . ( Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , Monoid (Trace trace term value)
                , Ord (CellFor value)
                , Ord term
                , Ord value
                , Recursive term
                , Reducer (ConfigurationFor term value) (Trace trace term value)
                , MonadAddressable (LocationFor value) (TracingAnalysis trace (Evaluation term value) term value (TracingEffects trace term value))
                , MonadAnalysis (Evaluation term value (TracingEffects trace term value))
                , MonadValue value (TracingAnalysis trace (Evaluation term value) term value (TracingEffects trace term value))
                , Semigroup (CellFor value)
                )
              => term
              -> Final (TracingEffects trace term value) value
evaluateTrace = run @(TracingEffects trace term value) . runEvaluator . runEvaluation . runTracingAnalysis @trace . evaluateTerm


newtype TracingAnalysis (trace :: * -> *) underlying term value (effects :: [* -> *]) a
  = TracingAnalysis { runTracingAnalysis :: underlying effects a }
  deriving (Applicative, Functor, LiftEffect, Monad, MonadFail)

deriving instance (AnalysisTerm (underlying effects) ~ term, AnalysisValue (underlying effects) ~ value, MonadEvaluator (underlying effects)) => MonadEvaluator (TracingAnalysis trace underlying term value effects)

instance ( Corecursive term
         , Evaluatable (Base term)
         , FreeVariables term
         , LiftEffect underlying
         , Member (Writer (Trace trace term value)) effects
         , MonadAddressable (LocationFor value) (TracingAnalysis trace underlying term value effects)
         , MonadAnalysis (underlying effects)
         , AnalysisTerm (underlying effects) ~ term
         , AnalysisValue (underlying effects) ~ value
         , MonadValue value (TracingAnalysis trace underlying term value effects)
         , Recursive term
         , Reducer (ConfigurationFor term value) (Trace trace term value)
         , Semigroup (CellFor value)
         )
         => MonadAnalysis (TracingAnalysis trace underlying term value effects) where
  analyzeTerm term = getConfiguration (embedSubterm term) >>= trace . Reducer.unit >> TracingAnalysis (analyzeTerm (second runTracingAnalysis <$> term))

type instance AnalysisTerm (TracingAnalysis trace underlying term value effects) = term
type instance AnalysisValue (TracingAnalysis trace underlying term value effects) = value

trace :: (LiftEffect underlying, Member (Writer (Trace trace term value)) effects)
      => Trace trace term value
      -> TracingAnalysis trace underlying term value effects ()
trace w = lift (tell w)
