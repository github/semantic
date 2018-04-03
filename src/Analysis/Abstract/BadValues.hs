{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadValues where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Analysis.Abstract.Evaluating
import Data.Abstract.Environment as Env
import Prologue
import Data.ByteString.Char8 (pack)

newtype BadValues m (effects :: [* -> *]) a = BadValues (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m effects)                    => MonadControl term (BadValues m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (BadValues m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (BadValues m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (BadValues m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (BadValues m effects)

instance ( Effectful m
         , Member (Resumable (ValueError location value)) effects
         , Member (State (EvaluatingState location term value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis location term value (m effects)
         , MonadValue location value (BadValues m effects)
         )
      => MonadAnalysis location term value (BadValues m effects) where
  type Effects location term value (BadValues m effects) = State [Name] ': Effects location term value (m effects)

  analyzeTerm eval term = resumeException @(ValueError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> case error of
          (ScopedEnvironmentError _) -> do
            env <- getEnv
            yield (Env.push env)
          (CallError val) -> yield val
          (StringError val) -> yield (pack $ show val))

  analyzeModule = liftAnalyze analyzeModule
