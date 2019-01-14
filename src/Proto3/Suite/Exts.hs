{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingVia, PatternSynonyms, ScopedTypeVariables, UndecidableInstances #-}

module Proto3.Suite.Exts
  ( PrimitiveEnum (..)
  , pattern Present
  , pattern Absent
  , toByteString
  ) where

import Prologue

import Data.Either
import Proto3.Suite
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode
import Data.ByteString.Lazy (toStrict)

-- * The Nested newtype from proto3-suite is a useful type to represent
-- protobuf messages nested inside each other. Because all nested
-- fields are implicitly optional in proto3, Nested is a newtype over
-- Maybe—which is fine, except for the fact that it makes
-- pattern-matching a little tedious. Building pattern synonyms to
-- abstract over Nested Just and Nested Nothing values gives us
-- syntactic convenience while helping us avoid Maybe-blindness.

-- | Equivalent to @Nested (Just a)).
pattern Present :: a -> Nested a
pattern Present t = Nested (Just t)

-- | Equivalent to @Nested Nothing@.
pattern Absent :: Nested a
pattern Absent = Nested Nothing

{-# COMPLETE Present, Absent #-}

newtype PrimitiveEnum a = PrimitiveEnum a
  deriving (Eq, Ord, Bounded, Named, Enum, HasDefault)

-- | Provides a DerivingVia hook to opt into a sensible definition of 'Primitive'
-- for a given 'Enum'. Should the decoding fail, the 'HasDefault' instance is used
-- as a fallback.
instance (Enum a, Bounded a, Named a, HasDefault a) => Primitive (PrimitiveEnum a) where
  primType _ = Named (Single (nameOf (Proxy @a)))
  encodePrimitive = Encode.enum
  decodePrimitive = fromRight def <$> Decode.enum

toByteString :: Message a => a -> ByteString
toByteString = toStrict . toLazyByteString
