{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadModuleResolutions
( resumingBadModuleResolutions
) where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Prologue

resumingBadModuleResolutions :: Evaluator location term value (Resumable (ResolutionError value) ': effects) a -> Evaluator location term value effects a
resumingBadModuleResolutions = raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> yield  nameToResolve
  GoImportError pathToResolve     -> yield [pathToResolve]))
