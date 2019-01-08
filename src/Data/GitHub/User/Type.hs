{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, LambdaCase, ScopedTypeVariables #-}

module Data.GitHub.User.Type
  ( Type (..)
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

data Type
  = Unknown
  | User
  | Organization
  | Bot
    deriving (Eq, Show, Enum, Bounded, MessageField, Named, Generic)
    deriving Primitive via PrimitiveEnum Type

instance HasDefault Type where def = Unknown

instance Finite Type where
  enumerate = enumerateUpper "UNKNOWN" [User ..]
