{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Data.JSON.Fields
  ( JSONFields (..)
  , JSONFields1 (..)
  , ToJSONFields (..)
  , ToJSONFields1 (..)
  , (.=)
  , noChildren
  , withChildren
  ) where

import           Data.Aeson
import           Data.Sum (Apply (..), Sum)
import qualified Data.Text as Text
import           Prologue

class ToJSONFields a where
  toJSONFields :: KeyValue kv => a -> [kv]

class ToJSONFields1 f where
  toJSONFields1 :: (KeyValue kv, ToJSON a) => f a -> [kv]
  default toJSONFields1 :: (KeyValue kv, ToJSON a, GToJSONFields1 (Rep1 f), GConstructorName1 (Rep1 f), Generic1 f) => f a -> [kv]
  toJSONFields1 s = let r = from1 s in
    "term" .= gconstructorName1 r : gtoJSONFields1 r

withChildren :: (KeyValue kv, ToJSON a, Foldable f) => f a -> [kv] -> [kv]
withChildren f ks = ("children" .= toList f) : ks

noChildren :: KeyValue kv => [kv] -> [kv]
noChildren ks = ("children" .= ([] :: [Int])) : ks

instance ToJSONFields a => ToJSONFields (Join (,) a) where
  toJSONFields (Join (a, b)) = [ "before" .= object (toJSONFields a), "after" .= object (toJSONFields b) ]

instance ToJSONFields a => ToJSONFields (Maybe a) where
  toJSONFields = maybe [] toJSONFields

instance ToJSON a => ToJSONFields [a] where
  toJSONFields list = [ "children" .= list ]

instance ToJSONFields1 [] where
  toJSONFields1 list = [ "children" .= list ]

instance Apply ToJSONFields1 fs => ToJSONFields1 (Sum fs) where
  toJSONFields1 = apply @ToJSONFields1 toJSONFields1

instance (ToJSONFields a, ToJSONFields b) => ToJSONFields (a, b) where
  toJSONFields (a, b) = [ "before" .= JSONFields a, "after" .= JSONFields b ]


newtype JSONFields a = JSONFields { unJSONFields :: a }

instance ToJSONFields a => ToJSONFields (JSONFields a) where
  toJSONFields = toJSONFields . unJSONFields

instance ToJSONFields a => ToJSON (JSONFields a) where
  toJSON = object . toJSONFields . unJSONFields
  toEncoding = pairs . mconcat . toJSONFields . unJSONFields


newtype JSONFields1 f a = JSONFields1 { unJSONFields1 :: f a }

instance ToJSONFields1 f => ToJSONFields1 (JSONFields1 f) where
  toJSONFields1 = toJSONFields1 . unJSONFields1

instance (ToJSON a, ToJSONFields1 f) => ToJSONFields (JSONFields1 f a) where
  toJSONFields = toJSONFields1 . unJSONFields1

instance (ToJSON a, ToJSONFields1 f) => ToJSON (JSONFields1 f a) where
  toJSON = object . toJSONFields1 . unJSONFields1
  toEncoding = pairs . mconcat . toJSONFields1 . unJSONFields1


-- | A typeclass to retrieve the name of a data constructor.
class GConstructorName1 f where
  gconstructorName1 :: f a -> String

instance Apply GConstructorName1 fs => GConstructorName1 (Sum fs) where
  gconstructorName1 = apply @GConstructorName1 gconstructorName1

instance GConstructorName1 f => GConstructorName1 (M1 D c f) where
  gconstructorName1 = gconstructorName1 . unM1

instance Constructor c => GConstructorName1 (M1 C c f) where
  gconstructorName1 = conName

instance (GConstructorName1 f, GConstructorName1 g) => GConstructorName1 (f :+: g) where
  gconstructorName1 (L1 l) = gconstructorName1 l
  gconstructorName1 (R1 r) = gconstructorName1 r


-- | A typeclass to calculate a list of 'KeyValue's describing the record selector names and associated values on a datatype.
class GToJSONFields1 f where
  gtoJSONFields1 :: (KeyValue kv, ToJSON a) => f a -> [kv]

instance GToJSONFields1 f => GToJSONFields1 (M1 D c f) where
  gtoJSONFields1 = gtoJSONFields1 . unM1

instance GToJSONFields1 f => GToJSONFields1 (M1 C c f) where
  gtoJSONFields1 = gtoJSONFields1 . unM1

instance GToJSONFields1 U1 where
  gtoJSONFields1 _ = []

instance (Selector c, GSelectorJSONValue1 f) => GToJSONFields1 (M1 S c f) where
  gtoJSONFields1 m1 = case selName m1 of
    "" -> [ "children" .= json ]
    n ->  [ Text.pack n .= json ]
    where json = gselectorJSONValue1 (unM1 m1)

instance (GToJSONFields1 f, GToJSONFields1 g) => GToJSONFields1 (f :+: g) where
  gtoJSONFields1 (L1 l) = gtoJSONFields1 l
  gtoJSONFields1 (R1 r) = gtoJSONFields1 r

instance (GToJSONFields1 f, GToJSONFields1 g) => GToJSONFields1 (f :*: g) where
  gtoJSONFields1 (x :*: y) = gtoJSONFields1 x <> gtoJSONFields1 y

-- | A typeclass to retrieve the JSON 'Value' of a record selector.
class GSelectorJSONValue1 f where
  gselectorJSONValue1 :: ToJSON a => f a -> SomeJSON

instance GSelectorJSONValue1 Par1 where
  gselectorJSONValue1 = SomeJSON . unPar1

instance ToJSON1 f => GSelectorJSONValue1 (Rec1 f) where
  gselectorJSONValue1 = SomeJSON . SomeJSON1 . unRec1

instance ToJSON k => GSelectorJSONValue1 (K1 r k) where
  gselectorJSONValue1 = SomeJSON . unK1

-- | An existential type wrapping an JSON-compatible data type.
data SomeJSON where
  SomeJSON :: ToJSON a => a -> SomeJSON

instance ToJSON SomeJSON where
  toJSON (SomeJSON a) = toJSON a
  toEncoding (SomeJSON a) = toEncoding a

data SomeJSON1 where
  SomeJSON1 :: (ToJSON1 f, ToJSON a) => f a -> SomeJSON1

instance ToJSON SomeJSON1 where
  toJSON (SomeJSON1 fa) = toJSON1 fa
  toEncoding (SomeJSON1 fa) = toEncoding1 fa
