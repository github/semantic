{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.BadValues where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Analysis.Abstract.Evaluating
import Data.Abstract.Environment as Env
import Prologue

newtype BadValues m term value (effects :: [* -> *]) a = BadValues (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m term value effects) => MonadControl term (BadValues m term value effects)
deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (BadValues m term value effects)
deriving instance MonadHeap value (m term value effects) => MonadHeap value (BadValues m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (BadValues m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (BadValues m term value effects)

instance ( Effectful (m term value)
         , Member (Resumable (ValueExc value)) effects
         , Member (State (EvaluatingState term value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis term value (m term value effects)
         , MonadValue value (BadValues m term value effects)
         )
      => MonadAnalysis term value (BadValues m term value effects) where
  type RequiredEffects term value (BadValues m term value effects) = State [Name] ': RequiredEffects term value (m term value effects)

  analyzeTerm eval term = resumeException @(ValueExc value) (liftAnalyze analyzeTerm eval term) (
        \yield (ScopedEnvironmentError _) ->
          do
            env <- getEnv
            yield (Env.push env))

  analyzeModule = liftAnalyze analyzeModule
