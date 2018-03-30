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
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh, MonadNonDet)

deriving instance MonadControl term (m effects)                    => MonadControl term (Quietly m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (Quietly m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (Quietly m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (Quietly m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (Quietly m effects)

instance ( Effectful m
         , Member (Resumable (Unspecialized value)) effects
         , MonadAnalysis term value (m effects)
         , MonadValue (LocationFor value) value (Quietly m effects)
         )
      => MonadAnalysis term value (Quietly m effects) where
  type Effects term value (Quietly m effects) = Effects term value (m effects)

  analyzeTerm eval term = resumeException @(Unspecialized value) (liftAnalyze analyzeTerm eval term) (\yield (Unspecialized _) -> unit >>= yield)

  analyzeModule = liftAnalyze analyzeModule
