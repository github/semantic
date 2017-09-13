{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
module Data.JSON.Fields where

import Data.Aeson
import Data.Bifunctor.Join
import Data.Foldable (toList)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Proxy (Proxy(..))
import Data.Union

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

instance ToJSONFields1 [] where
  toJSONFields1 list = [ "children" .= list ]

instance (Apply1 Foldable fs) => ToJSONFields1 (Union fs) where
  toJSONFields1 = apply1 (Proxy :: Proxy Foldable) (\ r -> [ "children" .= toList r ])

instance (ToJSONFields a, ToJSONFields b) => ToJSONFields (a, b) where
  toJSONFields (a, b) = [ "before" .= JSONFields a, "after" .= JSONFields b ]

instance (ToJSONFields1 f, ToJSONFields1 g) => ToJSONFields1 (Sum f g) where
  toJSONFields1 (InL l) = toJSONFields1 l
  toJSONFields1 (InR r) = toJSONFields1 r


instance ToJSONFields a => ToJSONFields1 (Const a) where
  toJSONFields1 = toJSONFields . getConst

instance ToJSONFields1 Identity where
  toJSONFields1 (Identity a) = [ "recur" .= a ]


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
