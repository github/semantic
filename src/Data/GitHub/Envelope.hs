{-# LANGUAGE DerivingVia, DeriveAnyClass #-}

module Data.GitHub.Envelope
  ( Site (..)
  , Envelope (..)
  , fromEnvelopeByteString
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts
import qualified Proto3.Wire.Decode as Proto3

import Proto3.Google.Timestamp

data Site
  = Unknown
  | Localhost
  | CP1_IAD
  | SDC42_SEA
    deriving (Eq, Show, Ord, Enum, Bounded, Generic, MessageField, Named)
    deriving Primitive via PrimitiveEnum Site

instance HasDefault Site where def = minBound

data Envelope = Envelope
  { envelopeTypeURL   :: Text
  , envelopeMessage   :: ByteString
  , envelopeId        :: Text
  , envelopeTimestamp :: Nested Timestamp
  , envelopeSite      :: Site
  } deriving (Eq, Show, Generic, Named, Message)

fromEnvelopeByteString :: Message a => ByteString -> Either Proto3.ParseError a
fromEnvelopeByteString b = fromByteString b >>= fromByteString . envelopeMessage
