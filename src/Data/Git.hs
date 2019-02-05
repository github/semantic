{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

-- | Core data types for Git repositories, suitable for
-- sharing between different storage backends.
module Data.Git
  ( OID (..)
  , Ref (..)
  , SHA (..)
  , nullSHA
  ) where

import Prologue

import Proto3.Suite

newtype OID = OID Text
  deriving stock (Eq, Show, Ord)
  deriving newtype (MessageField, Primitive, NFData)

newtype Ref = Ref Text
  deriving stock (Eq, Show, Ord)
  deriving newtype (MessageField, Primitive, NFData)

newtype SHA = SHA Text
  deriving stock (Eq, Show, Ord)
  deriving newtype (MessageField, Primitive, NFData)

nullSHA :: SHA
nullSHA = SHA mempty
