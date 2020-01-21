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
  marshal :: (ToJSON a) => t a -> Value -- don't need default signature because they're the same now
  marshal = object . fields []
  fields :: (ToJSON a) => [(Text, Value)] -> t a -> [(Text, Value)]
  default fields :: ( Generic1 t, GFields (Rep1 t), ToJSON a) => [(Text, Value)] -> t a -> [(Text, Value)]
  fields acc = gfields acc . from1

-- Create MarshalJSON instances for each type constructor
instance (GMarshalJSON (Rep1 t), Generic1 t) => MarshalJSON t

-- Typeclass to generically marshal ASTs into JSON
class GMarshalJSON f where
  gmarshal :: (ToJSON a) => f a -> Value

-- Stores meta-data for datatypes
instance (GFields f, Datatype c) => GFields (M1 D c f) where
   gfields acc x = gfields ((Text.pack "type", String (Text.pack (datatypeName x))): acc) $ unM1 x
  -- gmarshal = gmarshal . unM1 -- using unM1 instead of pattern-matching on M1 in order to express with function composition

-- Need to know constructor names in order to distinguish between two AST datatypes which both have an extraChildren field. 
-- Maybe a type field?

-- 1. Need to get the name of the datatype. 
-- 2. Pass info along somewhere where we can do something with it. 

-- Fold over S1 product types and pass the result to Aeson objects
instance GFields fields => GFields (C1 c fields) where
  gfields acc x = gfields acc (unM1 x)
-- TODO: we first see gfields appear where we have constructors because ...

-- Implement base case for products
-- To get a value out of this datum, we define another typeclass: @GValue@ with the method @gvalue@.
instance (GValue p, Selector s) => GFields (S1 s p) where
  gfields acc x = (Text.pack (selName x), gvalue (unM1 x)) : acc

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

-- breadcrumb, no longer relevant: This looks wrong! But GHC can infer that we only have a single instance class constraint above
instance (MarshalJSON t) => GValue (Rec1 t) where
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
