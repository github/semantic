{-# LANGUAGE DeriveAnyClass #-}

module Proto3.Google.Timestamp (Timestamp (..)) where

import Prologue

import Proto3.Suite

-- | Predefined timestamp message provided by Google. The schema can be found
-- <https://github.com/protocolbuffers/protobuf/blob/master/src/google/protobuf/timestamp.proto here>.
data Timestamp = Timestamp
  { timestampSeconds :: Int64
  , timestampNanos   :: Int32
  } deriving (Eq, Show, Generic, Message, Named, NFData)
