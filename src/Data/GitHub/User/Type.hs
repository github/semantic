{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, LambdaCase, ScopedTypeVariables #-}

module Data.GitHub.User.Type
  ( Type (..) ) where

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
  enumerate _ = [ ("UNKNOWN", 0)
                , ("USER", 1)
                , ("ORGANIZATION", 2)
                , ("BOT", 3)
                ]
