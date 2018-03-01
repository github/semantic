module Data.Output where

import Prologue
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, intercalate)
import Data.Text.Encoding (encodeUtf8)

class Monoid o => Output o where
  toOutput :: o -> ByteString

instance Output ByteString where
  toOutput s = s

instance Output [Text] where
  toOutput = encodeUtf8 . intercalate "\n"

instance Output (Map Text Value) where
  toOutput = toStrict . (<> "\n") . encode

instance Output [Value] where
  toOutput = toStrict . (<> "\n") . encode
