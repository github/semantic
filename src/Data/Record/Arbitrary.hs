{-# LANGUAGE DataKinds, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Record.Arbitrary where

import Data.Record
import Prologue
import Test.QuickCheck

instance Arbitrary fields => Arbitrary (Record '[fields]) where
  arbitrary = RCons <$> arbitrary <*> pure RNil

  shrink (RCons h t) = RCons <$> shrink h <*> pure t
