{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
module Data.JSON.Fields where

import Data.Aeson
import Data.Bifunctor.Join
import Data.Foldable (toList)
import Data.Proxy (Proxy(..))
import Data.Union
import GHC.Generics

class ToJSONFields a where
  toJSONFields :: KeyValue kv => a -> [kv]

class ToJSONFields1 f where
  toJSONFields1 :: (KeyValue kv, ToJSON a) => f a -> [kv]


instance ToJSONFields a => ToJSONFields (Join (,) a) where
  toJSONFields (Join (a, b)) = [ "before" .= object (toJSONFields a), "after" .= object (toJSONFields b) ]

instance ToJSONFields a => ToJSONFields (Maybe a) where
  toJSONFields = maybe [] toJSONFields

instance ToJSON a => ToJSONFields [a] where
  toJSONFields list = [ "children" .= list ]

instance (Apply1 Foldable fs) => ToJSONFields1 (Union fs) where
  toJSONFields1 = apply1 (Proxy :: Proxy Foldable) (\ r -> [ "children" .= toList r ])

instance (ToJSON a, ToJSON b) => ToJSONFields (a, b) where
  toJSONFields (a, b) = [ "before" .= a, "after" .= b ]

instance (ToJSONFields1 f, ToJSONFields1 g) => ToJSONFields1 (f :*: g) where
  toJSONFields1 (a :*: b) = [ "before" .= JSONFields1 a, "after" .= JSONFields1 b ]


newtype JSONFields a = JSONFields { unJSONFields :: a }

instance (ToJSONFields a) => ToJSON (JSONFields a) where
  toJSON = object . toJSONFields . unJSONFields
  toEncoding = pairs . mconcat . toJSONFields . unJSONFields


newtype JSONFields1 f a = JSONFields1 { unJSONFields1 :: f a }

instance (ToJSON a, ToJSONFields1 f) => ToJSON (JSONFields1 f a) where
  toJSON = object . toJSONFields1 . unJSONFields1
  toEncoding = pairs . mconcat . toJSONFields1 . unJSONFields1
