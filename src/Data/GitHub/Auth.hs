{-# LANGUAGE DeriveAnyClass, LambdaCase #-}

module Data.GitHub.Auth
  ( AuthTypes (..)
  ) where

import Prologue

import Proto3.Suite

-- | There is no Enum instance for this type, as the schema
-- does not describe a well-formed Enum instance (7 has no value).
-- As such, a Finite instance is also omitted, so we can't generate
-- this type with proto-gen. But that's fine, since it's already
-- specified elsewhere.
data AuthTypes
  = Unknown
  | Anon
  | IntegrationServerToServer
  | Basic
  | OAuth
  | JWT
  | PersonalAccessToken
  | IntegrationUserToServer
  | OAuthServerToServer
    deriving (Eq, Show, Generic, Named, MessageField)

instance HasDefault AuthTypes where def = Unknown

toSchemaInt :: AuthTypes -> Int
toSchemaInt = \case
  Unknown                   -> 0
  Anon                      -> 1
  IntegrationServerToServer -> 2
  Basic                     -> 3
  OAuth                     -> 4
  JWT                       -> 5
  PersonalAccessToken       -> 6
  IntegrationUserToServer   -> 8
  OAuthServerToServer       -> 9

fromSchemaInt :: Int -> AuthTypes
fromSchemaInt = \case
  0 -> Unknown
  1 -> Anon
  2 -> IntegrationServerToServer
  3 -> Basic
  4 -> OAuth
  5 -> JWT
  6 -> PersonalAccessToken
  7 -> Unknown -- not specified in the schema
  8 -> IntegrationUserToServer
  9 -> OAuthServerToServer
  _ -> Unknown

instance Primitive AuthTypes where
  encodePrimitive i = encodePrimitive i . toSchemaInt
  decodePrimitive = fromSchemaInt <$> decodePrimitive
