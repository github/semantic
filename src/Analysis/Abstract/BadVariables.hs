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

deriving instance MonadControl term (m effects)                    => MonadControl term (BadVariables m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (BadVariables m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (BadVariables m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (BadVariables m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (BadVariables m effects)

instance ( Effectful m
         , Member (Resumable (EvalError value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis location term value (m effects)
         , MonadValue location value (BadVariables m effects)
         )
      => MonadAnalysis location term value (BadVariables m effects) where
  type Effects location term value (BadVariables m effects) = State [Name] ': Effects location term value (m effects)

  analyzeTerm eval term = resumeException @(EvalError value) (liftAnalyze analyzeTerm eval term) (
        \yield (FreeVariableError name) ->
            raise (modify' (name :)) >> unit >>= yield)

  analyzeModule = liftAnalyze analyzeModule
