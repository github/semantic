{-# LANGUAGE DeriveAnyClass, DerivingVia, LambdaCase #-}

module Data.GitHub.Auth
  ( AuthTypes (..)
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

data AuthTypes
  = Unknown
  | Anon
  | IntegrationServerToServer
  | Basic
  | OAuth
  | JWT
  | PersonalAccessToken
  | ReservedAuthType -- not specified in the .proto file
  | IntegrationUserToServer
  | OAuthServerToServer
    deriving (Eq, Show, Ord, Enum, Bounded, Generic, Named, MessageField)
    deriving Primitive via PrimitiveEnum AuthTypes

instance HasDefault AuthTypes where def = Unknown

