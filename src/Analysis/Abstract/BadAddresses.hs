{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadAddresses where

import Control.Abstract.Analysis
import Prologue

newtype BadAddresses m (effects :: [* -> *]) a = BadAddresses (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad, MonadFail, MonadFresh)

deriving instance MonadHeap location value effects m             => MonadHeap location value effects (BadAddresses m)
deriving instance MonadModuleTable location term value effects m => MonadModuleTable location term value effects (BadAddresses m)
deriving instance MonadEvaluator location term value effects m   => MonadEvaluator location term value effects (BadAddresses m)

instance ( Effectful m
         , Member (Resumable (AddressError location value)) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects (BadAddresses m)
         , Show location
         )
      => MonadAnalysis location term value effects (BadAddresses m) where
  type Effects location term value (BadAddresses m) = Effects location term value m

  analyzeTerm eval term = resume @(AddressError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("AddressError:" <> show error)
          case error of
            (UninitializedAddress _) -> hole >>= yield)

  analyzeModule = liftAnalyze analyzeModule
