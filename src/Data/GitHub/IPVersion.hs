{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DerivingVia #-}

module Data.GitHub.IPVersion
  ( IPVersion (..)
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

data IPVersion
  = Unknown
  | IPV4
  | IPV6
    deriving (Eq, Show, Enum, Bounded, MessageField, Named, Generic)
    deriving Primitive via PrimitiveEnum IPVersion

instance HasDefault IPVersion where def = Unknown
instance Finite IPVersion where enumerate = enumerateUpper "VERSION_UNKNOWN" [IPV4 ..]
