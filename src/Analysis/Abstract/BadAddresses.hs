{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.BadAddresses where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Prologue

newtype BadAddresses m (effects :: [* -> *]) a = BadAddresses { runBadAddresses :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadAddresses m)

instance ( Effectful m
         , Member (Resumable (AddressError location value)) effects
         , MonadAnalysis location term value effects m
         , AbstractHole value
         , Monoid (Cell location value)
         , Show location
         )
      => MonadAnalysis location term value effects (BadAddresses m) where
  analyzeTerm eval term = resume @(AddressError location value) (liftAnalyze analyzeTerm eval term) (
        \yield error -> do
          traceM ("AddressError:" <> show error)
          case error of
            UnallocatedAddress _ -> yield mempty
            UninitializedAddress _ -> yield hole)

  analyzeModule = liftAnalyze analyzeModule

instance ( Interpreter effects result rest m
         , MonadEvaluator location term value effects m
         , AbstractHole value
         , Monoid (Cell location value)
         )
      => Interpreter (Resumable (AddressError location value) ': effects) result rest (BadAddresses m) where
  interpret
    = interpret
    . runBadAddresses
    . raiseHandler (relay pure (\ (Resumable err) yield -> case err of
      UnallocatedAddress _ -> yield mempty
      UninitializedAddress _ -> yield hole))
