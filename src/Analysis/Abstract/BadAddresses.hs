{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.Abstract.BadAddresses where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Prologue

newtype BadAddresses m (effects :: [* -> *]) a = BadAddresses (m effects a)
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadAddresses m)

instance ( Effectful m
         , Member (Resumable (AddressError location value)) effects
         , MonadAnalysis location term value effects m
         , MonadValue location value effects (BadAddresses m)
         , Monoid (Cell location value)
         , Show location
         )
      => MonadAnalysis location term value effects (BadAddresses m) where
  type Effects location term value (BadAddresses m) = Resumable (AddressError location value) ': Effects location term value m

  analyzeTerm eval term = resume @(AddressError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("AddressError:" <> show error)
          case error of
            UnallocatedAddress _ -> yield mempty
            UninitializedAddress _ -> hole >>= yield)

  analyzeModule = liftAnalyze analyzeModule
