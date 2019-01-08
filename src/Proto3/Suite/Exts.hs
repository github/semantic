{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, ScopedTypeVariables, UndecidableInstances #-}

module Proto3.Suite.Exts
  ( PrimitiveEnum (..)
  ) where

import Prologue

import Proto3.Suite
import Proto3.Wire.Encode as Encode
import Proto3.Wire.Decode as Decode

newtype PrimitiveEnum a = PrimitiveEnum a
  deriving stock (Eq, Ord)
  deriving newtype (Bounded, Named, Enum, HasDefault)

-- | Provides a DerivingVia hook to opt into a sensible definition of 'Primitive'
-- for a given 'Enum'. Should the decoding fail, the 'HasDefault' instance is used
-- as a fallback.
instance forall a. (Enum a, Bounded a, Named a, HasDefault a) => Primitive (PrimitiveEnum a) where
  primType _ = Named (Single (nameOf (Proxy @a)))
  encodePrimitive = Encode.enum
  decodePrimitive = either (const def) id <$> Decode.enum
