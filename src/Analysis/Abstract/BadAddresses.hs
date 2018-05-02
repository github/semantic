{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- For the Interpreter instance’s Evaluator constraint
module Analysis.Abstract.BadAddresses where

import Control.Abstract.Analysis
import Data.Abstract.Address
import Prologue

newtype BadAddresses m (effects :: [* -> *]) a = BadAddresses { runBadAddresses :: m effects a }
  deriving (Alternative, Applicative, Functor, Effectful, Monad)

deriving instance MonadAnalysis location term value effects m => MonadAnalysis location term value effects (BadAddresses m)
deriving instance Evaluator location term value m => Evaluator location term value (BadAddresses m)

instance ( Interpreter m effects
         , Evaluator location term value m
         , AbstractHole value
         , Monoid (Cell location value)
         , Show location
         )
      => Interpreter (BadAddresses m) (Resumable (AddressError location value) ': effects) where
  type Result (BadAddresses m) (Resumable (AddressError location value) ': effects) result = Result m effects result
  interpret
    = interpret
    . runBadAddresses
    . raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("AddressError:" <> show err) *> case err of
      UnallocatedAddress _   -> yield mempty
      UninitializedAddress _ -> yield hole))
