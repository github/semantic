{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
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
newtype Quietly m (effects :: [* -> *]) a = Quietly (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadHeap location value effects m             => MonadHeap location value effects (Quietly m)
deriving instance MonadModuleTable location term value effects m => MonadModuleTable location term value effects (Quietly m)
deriving instance MonadEvaluator location term value effects m   => MonadEvaluator location term value effects (Quietly m)

instance ( Effectful m
         , Member (Resumable (Unspecialized value)) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects (Quietly m)
         )
      => MonadAnalysis location term value effects (Quietly m) where
  type Effects location term value (Quietly m) = Effects location term value m

  analyzeTerm eval term = resume @(Unspecialized value) (liftAnalyze analyzeTerm eval term) (\yield err@(Unspecialized _) ->
          traceM ("Unspecialized:" <> show err) >> hole >>= yield)

  analyzeModule = liftAnalyze analyzeModule
