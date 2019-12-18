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

module Marshal.JSON
( MarshalJSON(..)
) where

import Data.Aeson as Aeson
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as Text
-- import Marshal.Examples

-- Test datatype that will go away: this is just to get us started!
data Bar a = Bar
  { ann :: a
  , foo :: Text
  } deriving (Eq, Show, Generic1)
  -- Aeson requires that the datatypes derive Generic
  -- we want Generic1 because we can represent types of kind * -> *

-- Thinking about the shape of Bar:
-- it is a Datatype, meaning it will require M1 D
-- it has a data constructor, so it will require C1
-- it is a product type, so it will require :*:
-- it has record selector fields so it will require S1
-- is has a parameter so it will require Par1
-- it has a constant, foo :: Text, so it will require K1 R

-- Serialize unmarshaled ASTs into JSON representation by auto-deriving Aeson instances generically
-- Given some shape t with polymorphic type a such that a has a ToJSON instance, we can produce a JSON value
-- in the default definition, we add further constraints: 
-- - Generic1 t - the type constructor will be a sum, product, leaf etc. and will require a Generic1 instance
-- - GMarshalJSON (Rep1 t) -- the type constructor should be a Rep1, which is an associated type (ie., an open type family affiliated with a class, and thus it's like a function for types, in this case the Haskell language datatypes used in the AST)
class MarshalJSON t where
  marshal :: (ToJSON a) => t a -> Value
  default marshal :: ( Generic1 t, GMarshalJSON (Rep1 t), ToJSON a) => t a -> Value
  marshal = gmarshal . from1

-- We want to create MarshalJSON instances for each type constructor
-- if we wanted it for a particular instance, it'd be something like MarshalJSON Bar
instance (GMarshalJSON (Rep1 t), Generic1 t) => MarshalJSON t

-- Typeclass to generically marshal ASTs into JSON
-- Given some type @a@ that's an instance of @ToJSON@, we apply a function @f@ and return a JSON value represented as a Haskell value
class GMarshalJSON f where
  gmarshal :: (ToJSON a) => f a -> Value

-- Stores meta-data for datatypes
-- using unM1 instead of pattern-matching on M1 to express with function composition
instance GMarshalJSON f => GMarshalJSON (M1 D c f) where
  gmarshal = gmarshal . unM1

-- Need to fold over S1 product types and pass the result to Aeson objects
instance GFields fields => GMarshalJSON (C1 ('MetaCons ctorname x y) fields) where
  gmarshal = object . gfields [] . unM1

-- Implement the product case
instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields acc (f :*: g) = gfields (gfields acc g) f

-- Implement base case
-- Takes term-level value of the type-level string 'fieldname' by passing a Proxy specialised to 'fieldname' to the knownSymbol function.
-- To actually get a value out of this datum, we'll need one more typeclass. Let's call its method 'gvalue'.
instance (GValue p, Selector s) => GFields (S1 s p) where
  gfields acc x = (Text.pack (selName x), gvalue (unM1 x)) : acc
-- knows what the type of x is, whereas M1 has parameters that can be instantiated to anything

-- GValue for leaves
instance ToJSON a => GValue (K1 i a) where
  gvalue = toJSON . unK1

-- Par1 instance
instance GValue Par1 where
  gvalue = toJSON . unPar1

-- Define a new class to operate on product field types;
-- Takes an accumulator, a datatype, and returns a new accumulator value.
class GFields f where
  gfields :: ToJSON a => [(Text, Value)] -> f a -> [(Text, Value)]

-- gvalue is a wrappe`r that calls to @toJSON@ (for leaf node types such as Text) or recurses via @marshal@
-- since it's a function on types, we need a typeclass.
class GValue f where
  gvalue :: (ToJSON a) => f a -> Value

-- TODO: use toEncoding -- direct serialization to ByteString
