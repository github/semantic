{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s MonadEvaluator constraint
module Analysis.Abstract.BadAddresses where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Prologue

newtype BadAddresses m (effects :: [* -> *]) a = BadAddresses { runBadAddresses :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadEvaluator location term value effects m => MonadEvaluator location term value effects (BadAddresses m)
deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadAddresses m)

instance ( Interpreter effects m
         , MonadEvaluator location term value effects m
         , AbstractHole value
         , Monoid (Cell location value)
         , Show location
         )
      => Interpreter (Resumable (AddressError location value) ': effects) (BadAddresses m) where
  type Result (Resumable (AddressError location value) ': effects) (BadAddresses m) result = Result effects m result
  interpret
    = interpret
    . runBadAddresses
    . raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("AddressError:" <> show err) *> case err of
      UnallocatedAddress _   -> yield mempty
      UninitializedAddress _ -> yield hole))
