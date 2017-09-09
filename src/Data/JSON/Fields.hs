module Data.JSON.Fields where

import Data.Aeson
import Data.Bifunctor.Join

class ToJSONFields a where
  toJSONFields :: KeyValue kv => a -> [kv]


instance ToJSONFields a => ToJSONFields (Join (,) a) where
  toJSONFields (Join (a, b)) = [ "before" .= object (toJSONFields a), "after" .= object (toJSONFields b) ]
