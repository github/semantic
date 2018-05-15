{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Data.JSON.Fields
  ( JSONFields (..)
  , JSONFields1 (..)
  , ToJSONFields (..)
  , ToJSONFields1 (..)
  , (.=)
  , noChildren
  , withChildren
  ) where

import Data.Aeson
import Data.Sum (Apply(..), Sum)
import Prologue

class ToJSONFields a where
  toJSONFields :: KeyValue kv => a -> [kv]

class ToJSONFields1 f where
  toJSONFields1 :: (KeyValue kv, ToJSON a) => f a -> [kv]
  default toJSONFields1 :: (KeyValue kv, ToJSON a, Foldable f) => f a -> [kv]
  toJSONFields1 f = ["children" .= toList f]

withChildren :: (KeyValue kv, ToJSON a, Foldable f) => f a -> [kv] -> [kv]
withChildren f ks = ("children" .= toList f) : ks

noChildren :: KeyValue kv => [kv] -> [kv]
noChildren ks = ("children" .= ([] :: String)) : ks

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
