{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Marshal.JSON
( MarshalJSON(..)
) where

import           AST.Parse
import           Data.Aeson as Aeson hiding (Success)
import           Data.Bifunctor (first)
import           Data.List.NonEmpty (NonEmpty)
import           Data.String (fromString)
import           Data.Text (Text, unpack)
import qualified Data.Text as Text
import           GHC.Generics

-- TODO: use toEncoding -- direct serialization to ByteString

-- Serialize unmarshaled ASTs into JSON representation by auto-deriving Aeson instances generically
class MarshalJSON t where
  marshal :: (ToJSON a) => t a -> Value
  marshal = object . map (first (fromString . unpack)) . fields []
  fields :: (ToJSON a) => [(Text, Value)] -> t a -> [(Text, Value)]
  default fields :: ( Generic1 t, GFields (Rep1 t), ToJSON a) => [(Text, Value)] -> t a -> [(Text, Value)]
  fields acc = gfields acc . from1

-- Implement the sum case
instance {-# OVERLAPPING #-} (MarshalJSON f, MarshalJSON g) => MarshalJSON (f :+: g) where
  fields acc (L1 f) = fields acc f
  fields acc (R1 g) = fields acc g

-- Create MarshalJSON instances for each type constructor
instance (GFields (Rep1 t), Generic1 t) => MarshalJSON t

-- Stores meta-data for datatypes
instance (GFields f, Datatype c) => GFields (M1 D c f) where
   gfields acc x = gfields ((Text.pack "type", String (Text.pack (datatypeName x))): acc) $ unM1 x

-- Fold over S1 product types and pass the result to Aeson objects
instance GFields fields => GFields (C1 c fields) where
  gfields acc x = gfields acc (unM1 x)

-- Implement base case for products
-- To get a value out of this datum, we define another typeclass: @GValue@ with the method @gvalue@.
instance (GValue p, Selector s) => GFields (S1 s p) where
  gfields acc x = (Text.pack (selName x), gvalue (unM1 x)) : acc

-- Implement inductive case for product case
-- Product datatypes are marshalled to an object with a type field holding the constructor name and a separate field for each selector in the datatype.
instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields acc (f :*: g) = gfields (gfields acc g) f

-- GValue for leaves
instance ToJSON a => GValue (K1 i a) where
  gvalue = toJSON . unK1

-- Par1 instance
instance GValue Par1 where
  gvalue = toJSON . unPar1

instance (MarshalJSON t) => GValue (Rec1 t) where
  gvalue (Rec1 f) = marshal f

instance (GValue t) => GValue (Maybe :.: t) where
  gvalue (Comp1 (Just t)) = gvalue t
  gvalue (Comp1 Nothing)  = Null

instance (GValue t) => GValue ([] :.: t) where
  gvalue (Comp1 ts) = toJSON $ map gvalue ts

instance (GValue t) => GValue (NonEmpty :.: t) where
  gvalue (Comp1 ts) = toJSON $ fmap gvalue ts

instance (GValue t) => GValue (Err :.: t) where
  gvalue (Comp1 (Success t)) = gvalue t
  gvalue (Comp1 (Fail _))    = Null

-- GFields operates on product field types: it takes an accumulator, a datatype, and returns a new accumulator value.
class GFields f where
  gfields :: ToJSON a => [(Text, Value)] -> f a -> [(Text, Value)]

-- gvalue is a wrapper that calls to @toJSON@ (for leaf nodes such as @Text@) or recurses via @marshal@
class GValue f where
  gvalue :: (ToJSON a) => f a -> Value
