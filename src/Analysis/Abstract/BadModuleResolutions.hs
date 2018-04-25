{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.BadModuleResolutions where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

newtype BadModuleResolutions m (effects :: [* -> *]) a = BadModuleResolutions { runBadModuleResolutions :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadModuleResolutions m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadModuleResolutions m)

instance ( Interpreter effects result rest m
         , MonadEvaluator location term value effects m
         )
      => Interpreter (Resumable (ResolutionError value) ': effects) result rest (BadModuleResolutions m) where
  interpret
    = interpret
    . runBadModuleResolutions
    . raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("ResolutionError:" <> show err) *> case err of
      RubyError       nameToResolve -> yield nameToResolve
      TypeScriptError nameToResolve -> yield nameToResolve))
