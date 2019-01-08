{-# LANGUAGE DeriveAnyClass #-}

module Proto3.Google.Wrapped
  ( Wrapped (..)
  ) where

import Prologue

import Proto3.Suite

newtype Wrapped a = Wrapped { value :: a }
  deriving (Eq, Show, Generic)

instance Named (Wrapped Text) where nameOf _ = "StringValue"

deriving instance Message (Wrapped Text)
