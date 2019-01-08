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

-- | Piggybacks on top of the 'Enumerated' instance, as the generated code would.
-- This instance will get easier when we have DerivingVia.
instance forall a. (Enum a, Bounded a, Named a, HasDefault a) => Primitive (PrimitiveEnum a) where
  primType _ = Named (Single (nameOf (Proxy @a)))
  encodePrimitive = Encode.enum
  decodePrimitive = either (const def) id <$> Decode.enum

enumerateUpper :: (IsString string, Show a, Enum a) => string -> [a] -> Proxy a -> [(string, Int)]
enumerateUpper msg as _ = (msg, 0) : fmap (\a -> (go a, fromEnum a)) as
  where go = fromString . fmap toUpper . show
