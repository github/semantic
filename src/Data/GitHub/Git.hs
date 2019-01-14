{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Data.GitHub.Git
  ( OID (..)
  , Ref (..)
  , SHA (..)
  ) where

import Prologue

import Proto3.Suite

newtype OID = OID Text
  deriving stock (Eq, Show, Ord)
  deriving newtype (MessageField, Primitive)

newtype Ref = Ref Text
  deriving stock (Eq, Show, Ord)
  deriving newtype (MessageField, Primitive)

newtype SHA = SHA Text
  deriving stock (Eq, Show, Ord)
  deriving newtype (MessageField, Primitive)
