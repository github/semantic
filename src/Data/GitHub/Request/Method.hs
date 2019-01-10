{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Data.GitHub.Request.Method
  ( RequestMethod (..)
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

data RequestMethod
  = Unknown
  | Get
  | Post
  | Put
  | Delete
  | Update
  | Options
  | Connect
  | Head
  | Patch
    deriving (Eq, Show, Ord, Enum, Bounded, Generic, Named, MessageField)
    deriving Primitive via PrimitiveEnum RequestMethod

instance HasDefault RequestMethod where def = Unknown
