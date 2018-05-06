{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadSyntax
( resumingBadSyntax
) where

import Control.Abstract.Evaluator
import Data.Abstract.Evaluatable
import Prologue

-- | An analysis which resumes 'Unspecialized' exceptions instead of failing.
--
--   Use it by composing it onto an analysis:
--
--   > resumingBadSyntax . …
resumingBadSyntax :: AbstractHole value => Evaluator location term value (Resumable (Unspecialized value) ': effects) a -> Evaluator location term value effects a
resumingBadSyntax = runUnspecializedWith (\ err@(Unspecialized _) -> traceM ("Unspecialized:" <> show err) *> pure hole)
