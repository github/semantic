{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s Evaluator constraint
module Analysis.Abstract.BadModuleResolutions where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

newtype BadModuleResolutions m (effects :: [* -> *]) a = BadModuleResolutions { runBadModuleResolutions :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadModuleResolutions m)
deriving instance Evaluator location term value m => Evaluator location term value (BadModuleResolutions m)

instance ( Interpreter m effects
         , Evaluator location term value m
         )
      => Interpreter (BadModuleResolutions m) (Resumable (ResolutionError value) ': effects) where
  type Result (BadModuleResolutions m) (Resumable (ResolutionError value) ': effects) result = Result m effects result
  interpret
    = interpret
    . runBadModuleResolutions
    . raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("ResolutionError:" <> show err) *> case err of
      NotFoundError nameToResolve _ _ -> yield  nameToResolve
      GoImportError pathToResolve     -> yield [pathToResolve]))
