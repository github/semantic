{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module Marshal.JSON
( MarshalJSON(..)
) where

import Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as Text

-- Serialize unmarshaled ASTs into JSON representation by auto-deriving Aeson instances generically
class MarshalJSON t where
  marshal :: (ToJSON a) => t a -> Value
  default marshal :: ( Generic1 t, GMarshalJSON (Rep1 t), ToJSON a) => t a -> Value
  marshal = gmarshal . from1

-- Create MarshalJSON instances for each type constructor
instance (GMarshalJSON (Rep1 t), Generic1 t) => MarshalJSON t

-- Typeclass to generically marshal ASTs into JSON
class GMarshalJSON f where
  gmarshal :: (ToJSON a) => f a -> Value

-- Stores meta-data for datatypes
instance GMarshalJSON f => GMarshalJSON (M1 D c f) where
  gmarshal = gmarshal . unM1 -- using unM1 instead of pattern-matching on M1 in order to express with function composition

-- Fold over S1 product types and pass the result to Aeson objects
instance GFields fields => GMarshalJSON (C1 c fields) where
  gmarshal = object . gfields [] . unM1
-- TODO: we first see gfields appear where we have constructors because ...

-- Implement base case for products
-- To get a value out of this datum, we define another typeclass: @GValue@ with the method @gvalue@.
instance (GValue p, Selector s) => GFields (S1 s p) where
  gfields acc x = (Text.pack (selName x), gvalue (unM1 x)) : acc
-- knows what the type of x is, whereas M1 has parameters that can be instantiated to anything


-- Implement inductive case for product case
instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields acc (f :*: g) = gfields (gfields acc g) f

-- Implement the sum case
instance (GMarshalJSON f, GMarshalJSON g) => GMarshalJSON (f :+: g) where
  gmarshal (L1 f) = gmarshal f
  gmarshal (R1 g) = gmarshal g

-- GValue for leaves
instance ToJSON a => GValue (K1 i a) where
  gvalue = toJSON . unK1

-- Par1 instance
instance GValue Par1 where
  gvalue = toJSON . unPar1

instance (GMarshalJSON (Rep1 t), Generic1 t) => GValue (Rec1 t) where
  gvalue (Rec1 f) = marshal f

instance (GValue t) => GValue (Maybe :.: t) where
  gvalue (Comp1 (Just t)) = gvalue t
  gvalue (Comp1 Nothing) = Null

instance (GValue t) => GValue ([] :.: t) where
  gvalue (Comp1 ts) = toJSON $ map gvalue ts

instance (GValue t) => GValue (NonEmpty :.: t) where
  gvalue (Comp1 ts) = toJSON $ fmap gvalue ts

-- Define a new class to operate on product field types;
-- Takes an accumulator, a datatype, and returns a new accumulator value.
class GFields f where
  gfields :: ToJSON a => [(Text, Value)] -> f a -> [(Text, Value)]

-- gvalue is a wrapper that calls to @toJSON@ (for leaf node types such as Text) or recurses via @marshal@
-- since it's a function on types, we need a typeclass
class GValue f where
  gvalue :: (ToJSON a) => f a -> Value

-- TODO: use toEncoding -- direct serialization to ByteString
