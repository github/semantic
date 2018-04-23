{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadModuleResolutions where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Analysis.Abstract.Evaluating
import Prologue

newtype BadModuleResolutions m (effects :: [* -> *]) a = BadModuleResolutions (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadControl term (m effects)                    => MonadControl term (BadModuleResolutions m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (BadModuleResolutions m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (BadModuleResolutions m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (BadModuleResolutions m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (BadModuleResolutions m effects)

instance ( Effectful m
         , Member (Resumable (ResolutionError value)) effects
         , Member (State (EvaluatingState location term value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis location term value (m effects)
         , MonadValue location value (BadModuleResolutions m effects)
         )
      => MonadAnalysis location term value (BadModuleResolutions m effects) where
  type Effects location term value (BadModuleResolutions m effects) = State [Name] ': Effects location term value (m effects)

  analyzeTerm eval term = resume @(ResolutionError value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("ResolutionError:" <> show error)
          case error of
            (NotFoundError nameToResolve _ _) -> yield nameToResolve)

  analyzeModule = liftAnalyze analyzeModule
