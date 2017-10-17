module Data.Output where

import Data.Aeson (Value, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Map (Map)
import Data.Semigroup
import Data.Text (Text)
import Data.ByteString (intercalate)

class Monoid o => Output o where
  toOutput :: o -> ByteString

instance Output ByteString where
  toOutput s = s

instance Output [ByteString] where
  toOutput = intercalate "\n"

instance Output (Map Text Value) where
  toOutput = toStrict . (<> "\n") . encode

instance Output [Value] where
  toOutput = toStrict . (<> "\n") . encode
