{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadAddresses where

import Control.Abstract.Analysis
import Data.Abstract.Evaluatable
import Analysis.Abstract.Evaluating
import Prologue

newtype BadAddresses m (effects :: [* -> *]) a = BadAddresses (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadControl term (m effects)                    => MonadControl term (BadAddresses m effects)
deriving instance MonadEnvironment location value (m effects)      => MonadEnvironment location value (BadAddresses m effects)
deriving instance MonadHeap location value (m effects)             => MonadHeap location value (BadAddresses m effects)
deriving instance MonadModuleTable location term value (m effects) => MonadModuleTable location term value (BadAddresses m effects)
deriving instance MonadEvaluator location term value (m effects)   => MonadEvaluator location term value (BadAddresses m effects)

instance ( Effectful m
         , Member (Resumable (AddressError location value)) effects
         , Member (State (EvaluatingState location term value)) effects
         , MonadAnalysis location term value (m effects)
         , MonadValue location value (BadAddresses m effects)
         , Show location
         )
      => MonadAnalysis location term value (BadAddresses m effects) where
  type Effects location term value (BadAddresses m effects) = Effects location term value (m effects)

  analyzeTerm eval term = resumeException @(AddressError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("AddressError:" <> show error)
          case error of
            (UninitializedAddress address) -> hole >>= yield)

  analyzeModule = liftAnalyze analyzeModule
