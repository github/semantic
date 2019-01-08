{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, LambdaCase, ScopedTypeVariables, UndecidableInstances #-}

module Proto3.Suite.Exts
  ( PrimitiveEnum (..)
  , enumerateUpper
  ) where

import Prologue

import Data.Char (toUpper)
import Data.String

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

-- | A convenience function for declaring 'Finite' instances. Please note that this does
-- not map CamelCase to LOUD_SNAKE_CASE, though it will work fine with single-word constructors.
enumerateUpper :: (IsString string, Show a, Enum a)
               => string   -- ^ The name of the first field, e.g. "UNKNOWN_MYTYPE. Has value 0.
               -> [a]      -- ^ Subsequent entries to use; the names will be 'Show'n and capitalized.
               -> Proxy a
               -> [(string, Int)]
enumerateUpper msg as _ = (msg, 0) : fmap (\a -> (go a, fromEnum a)) as
  where go = fromString . fmap toUpper . show
