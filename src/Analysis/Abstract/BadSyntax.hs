{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.BadSyntax
( BadSyntax
) where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

-- | An analysis which resumes exceptions instead of failing.
--
--   Use it by composing it onto an analysis:
--
--   > runAnalysis @(BadSyntax (Evaluating term value)) (…)
--
--   Note that exceptions thrown by other analyses may not be caught if 'BadSyntax' doesn’t know about them, i.e. if they’re not part of the generic 'MonadValue', 'MonadAddressable', etc. machinery.
newtype BadSyntax m (effects :: [* -> *]) a = BadSyntax { runBadSyntax :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadSyntax m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadSyntax m)

instance ( Interpreter effects m
         , MonadEvaluator location term value effects m
         , AbstractHole value
         )
      => Interpreter (Resumable (Unspecialized value) ': effects) (BadSyntax m) where
  type Result (Resumable (Unspecialized value) ': effects) (BadSyntax m) result = Result effects m result
  interpret
    = interpret
    . runBadSyntax
    . raiseHandler (relay pure (\ (Resumable err@(Unspecialized _)) yield -> traceM ("Unspecialized:" <> show err) *> yield hole))
