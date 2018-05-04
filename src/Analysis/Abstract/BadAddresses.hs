{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadAddresses
( resumingBadAddresses
) where

import Control.Abstract.Addressable
import Control.Abstract.Evaluator
import Control.Abstract.Value
import Data.Abstract.Address
import Prologue

resumingBadAddresses :: (AbstractHole value, Monoid (Cell location value), Show location) => Evaluator location term value (Resumable (AddressError location value) ': effects) a -> Evaluator location term value effects a
resumingBadAddresses = raiseHandler (relay pure (\ (Resumable err) yield -> traceM ("AddressError:" <> show err) *> case err of
  UnallocatedAddress _   -> yield mempty
  UninitializedAddress _ -> yield hole))
