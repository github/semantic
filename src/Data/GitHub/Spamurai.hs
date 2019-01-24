{-# LANGUAGE DerivingVia, DeriveAnyClass #-}

module Data.GitHub.Spamurai
  ( SpamuraiClassification (..)
  ) where

import Prologue

import Proto3.Suite

data SpamuraiClassification
  = Unknown
  | Hammy
  | Spammy
    deriving (Eq, Show, Enum, Bounded, Generic, MessageField, Named)
    deriving Primitive via PrimitiveEnum SpamuraiClassification

instance HasDefault SpamuraiClassification where def = Unknown
