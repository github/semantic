{-# LANGUAGE GADTs, TypeOperators #-}
module Analysis.Abstract.BadAddresses
( resumingBadAddresses
) where

import Control.Abstract.Addressable
import Control.Abstract.Evaluator
import Control.Abstract.Value
import Data.Abstract.Address
import Data.Semilattice.Lower
import Prologue

resumingBadAddresses :: (AbstractHole value, Lower (Cell location value), Show location) => Evaluator location term value (Resumable (AddressError location value) ': effects) a -> Evaluator location term value effects a
resumingBadAddresses = runAddressErrorWith (\ err -> traceM ("AddressError:" <> show err) *> case err of
  UnallocatedAddress _   -> pure lowerBound
  UninitializedAddress _ -> pure hole)
