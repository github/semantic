{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.BadModuleResolutions where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

newtype BadModuleResolutions m (effects :: [* -> *]) a = BadModuleResolutions (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadModuleResolutions m)

instance ( Effectful m
         , Member (Resumable (ResolutionError value)) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects (BadModuleResolutions m)
         )
      => MonadAnalysis location term value effects (BadModuleResolutions m) where
  analyzeTerm eval term = resume @(ResolutionError value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("ResolutionError:" <> show error)
          case error of
            RubyError nameToResolve -> yield nameToResolve
            TypeScriptError nameToResolve -> yield nameToResolve)

  analyzeModule = liftAnalyze analyzeModule

instance ( Interpreter effects result rest m
         , MonadEvaluator location term value effects m
         )
      => Interpreter (Resumable (ResolutionError value) ': effects) result rest (BadModuleResolutions m) where
  interpret = interpret . raise @m . relay pure (\ (Resumable err) yield -> case err of
    RubyError nameToResolve -> yield nameToResolve
    TypeScriptError nameToResolve -> yield nameToResolve) . lower
