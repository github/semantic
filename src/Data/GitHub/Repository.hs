{-# LANGUAGE DerivingVia, DeriveAnyClass #-}

module Data.GitHub.Repository
  ( Repository (..)
  , Visibility (..)
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

import Proto3.Google.Timestamp

data Visibility
  = Unknown
  | Public
  | Private
    deriving (Eq, Show, Enum, Bounded, Generic, MessageField, Named)
    deriving Primitive via PrimitiveEnum Visibility

instance HasDefault Visibility where def = Unknown

data Repository = Repository
  { repoId              :: Int32
  , repoGlobalRelayId   :: Text
  , repoName            :: Text
  , repoDescription     :: Text
  , repoVisibility      :: Visibility
  , repoParentId        :: Word32
  , repoStargazerCount  :: Word32
  , repoPublicForkCount :: Word32
  , repoPushedAt        :: Nested Timestamp
  , repoCreatedAt       :: Nested Timestamp
  , repoUpdatedAt       :: Nested Timestamp
  } deriving (Eq, Show, Generic, Message, Named)
