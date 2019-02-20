{-# LANGUAGE DeriveAnyClass #-}

module Proto3.Google.Wrapped
  ( Wrapped (..)
  ) where

import Prologue

import Proto3.Suite

-- | Because protobuf primitive types (string, int32, etc.) are not nullable, Google provides a set of standard
-- <https://github.com/protocolbuffers/protobuf/blob/master/src/google/protobuf/wrappers.proto wrappers>
-- to create messages around each primitive type. Because we have nice things in Haskell, we don't need to define
-- a separate data type for each of these wrappers: we just need to declare 'Named' instances for each instance
-- for which we want to link the Google-named wrapped types.
newtype Wrapped a = Wrapped { value :: a }
  deriving (Eq, Show, Ord, Generic, NFData)

instance Named (Wrapped Text) where nameOf _ = "StringValue"
instance Named (Wrapped ByteString) where nameOf _ = "BytesValue"
instance Named (Wrapped Double) where nameOf _ = "DoubleValue"
instance Named (Wrapped Int64) where nameOf _ = "Int64Value"
instance Named (Wrapped Word64) where nameOf _ = "UInt64Value"
instance Named (Wrapped Int32) where nameOf _ = "Int32Value"
instance Named (Wrapped Word32) where nameOf _ = "UInt32Value"
instance Named (Wrapped Bool) where nameOf _ = "BoolValue"

deriving instance MessageField a => Message (Wrapped a)
