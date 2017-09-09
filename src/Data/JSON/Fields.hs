module Data.JSON.Fields where

import Data.Aeson

class ToJSONFields a where
  toJSONFields :: KeyValue kv => a -> [kv]
