{-# LANGUAGE DeriveAnyClass #-}

module Data.GitHub.Timestamp (Timestamp (..)) where

import Prologue

import Proto3.Suite

data Timestamp = Timestamp
  { timestampSeconds :: Int64
  , timestampNanos   :: Int32
  } deriving (Eq, Show, Generic, Message, Named)
