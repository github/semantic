{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DerivingVia #-}

module Data.GitHub.IPVersion
  ( IPVersion (..)
  ) where

import Prologue

import Proto3.Suite

data IPVersion
  = Unknown
  | IPV4
  | IPV6
    deriving (Eq, Show, Enum, Bounded, MessageField, Named, Generic)
    deriving Primitive via PrimitiveEnum IPVersion

instance HasDefault IPVersion where def = Unknown
