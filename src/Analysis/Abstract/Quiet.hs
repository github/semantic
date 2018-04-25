{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.Quiet
( Quietly
) where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Prologue

-- | An analysis which resumes exceptions instead of failing.
--
--   Use it by composing it onto an analysis:
--
--   > runAnalysis @(Quietly (Evaluating term value)) (…)
--
--   Note that exceptions thrown by other analyses may not be caught if 'Quietly' doesn’t know about them, i.e. if they’re not part of the generic 'MonadValue', 'MonadAddressable', etc. machinery.
newtype Quietly m (effects :: [* -> *]) a = Quietly { runQuietly :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (Quietly m)

instance ( Effectful m
         , Member (Resumable (Unspecialized value)) effects
         , MonadAnalysis location term value effects m
         , MonadHole value effects (Quietly m)
         )
      => MonadAnalysis location term value effects (Quietly m) where
  analyzeTerm eval term = resume @(Unspecialized value) (liftAnalyze analyzeTerm eval term) (\yield err@(Unspecialized _) ->
          traceM ("Unspecialized:" <> show err) >> hole >>= yield)

  analyzeModule = liftAnalyze analyzeModule

instance ( Interpreter effects result rest m
         , MonadEvaluator location term value effects m
         , MonadHole value effects m
         )
      => Interpreter (Resumable (Unspecialized value) ': effects) result rest (Quietly m) where
  interpret
    = interpret
    . runQuietly
    . raiseHandler (relay pure (\ (Resumable (Unspecialized _)) yield -> lower @m hole >>= yield))
