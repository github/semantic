{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadModuleResolutions
( resumingBadModuleResolutions
) where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Prologue

resumingBadModuleResolutions :: (Applicative (m effects), Effectful m) => m (Resumable ResolutionError ': effects) a -> m effects a
resumingBadModuleResolutions = runResolutionErrorWith (\ err -> traceM ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])
