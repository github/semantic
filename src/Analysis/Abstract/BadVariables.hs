{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
module Analysis.Abstract.BadVariables where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

newtype BadVariables m term value (effects :: [* -> *]) a = BadVariables (m term value effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m term value effects) => MonadControl term (BadVariables m term value effects)
deriving instance MonadEnvironment value (m term value effects) => MonadEnvironment value (BadVariables m term value effects)
deriving instance MonadHeap value (m term value effects) => MonadHeap value (BadVariables m term value effects)
deriving instance MonadModuleTable term value (m term value effects) => MonadModuleTable term value (BadVariables m term value effects)
deriving instance MonadEvaluator term value (m term value effects) => MonadEvaluator term value (BadVariables m term value effects)

instance ( Effectful (m term value)
         , Member (Resumable (EvalError value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis term value (m term value effects)
         , MonadValue value (BadVariables m term value effects)
         )
      => MonadAnalysis term value (BadVariables m term value effects) where
  type RequiredEffects term value (BadVariables m term value effects) = State [Name] ': RequiredEffects term value (m term value effects)

  analyzeTerm eval term = resumeException @(EvalError value) (liftAnalyze analyzeTerm eval term) (
        \yield (FreeVariableError name) ->
            raise (modify (name :)) >> unit >>= yield)

  analyzeModule = liftAnalyze analyzeModule
