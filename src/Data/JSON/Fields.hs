{-# LANGUAGE MultiParamTypeClasses #-}
module Data.JSON.Fields where

import Data.Aeson
import Data.Bifunctor.Join
import Data.Foldable (toList)
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

instance (Apply1 Foldable fs, ToJSON a) => ToJSONFields (Union fs a) where
  toJSONFields = apply1 (Proxy :: Proxy Foldable) (\ r -> [ "children" .= toList r ])
