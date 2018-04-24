{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadModuleResolutions where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

newtype BadModuleResolutions m (effects :: [* -> *]) a = BadModuleResolutions (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadModuleResolutions m)

instance ( Effectful m
         , Member (Resumable (ResolutionError value)) effects
         , Member (State [Name]) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects (BadModuleResolutions m)
         )
      => MonadAnalysis location term value effects (BadModuleResolutions m) where
  type Effects location term value (BadModuleResolutions m) = State [Name] ': Effects location term value m

  analyzeTerm eval term = resume @(ResolutionError value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("ResolutionError:" <> show error)
          case error of
            (NotFoundError nameToResolve _ _) -> yield nameToResolve
            (GoImportError pathToResolve) -> yield [pathToResolve])

  analyzeModule = liftAnalyze analyzeModule
