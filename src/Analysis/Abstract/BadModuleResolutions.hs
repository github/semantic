{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadModuleResolutions
( resumingBadModuleResolutions
) where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Prologue

resumingBadModuleResolutions :: Effectful m => m (Resumable ResolutionError ': effects) a -> m effects a
resumingBadModuleResolutions = raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> yield  nameToResolve
  GoImportError pathToResolve     -> yield [pathToResolve]))
