{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadVariables
( BadVariables
) where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

-- An analysis that resumes from evaluation errors and records the list of unresolved free variables.
newtype BadVariables m (effects :: [* -> *]) a = BadVariables (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m effects)           => MonadControl term (BadVariables m effects)
deriving instance MonadEnvironment value (m effects)      => MonadEnvironment value (BadVariables m effects)
deriving instance MonadHeap value (m effects)             => MonadHeap value (BadVariables m effects)
deriving instance MonadModuleTable term value (m effects) => MonadModuleTable term value (BadVariables m effects)
deriving instance MonadEvaluator term value (m effects)   => MonadEvaluator term value (BadVariables m effects)

instance ( Effectful m
         , Member (Resumable (EvalError value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis term value (m effects)
         , MonadValue value (BadVariables m effects)
         )
      => MonadAnalysis term value (BadVariables m effects) where
  type RequiredEffects term value (BadVariables m effects) = State [Name] ': RequiredEffects term value (m effects)

  analyzeTerm eval term = resumeException @(EvalError value) (liftAnalyze analyzeTerm eval term) (
        \yield (FreeVariableError name) ->
            raise (modify' (name :)) >> unit >>= yield)

  analyzeModule = liftAnalyze analyzeModule
