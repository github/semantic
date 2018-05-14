module Data.Output
( Output(..)
) where

import Data.Aeson (Value, fromEncoding, toEncoding)
import Data.ByteString.Builder (Builder, byteString)
import Prologue

class Monoid o => Output o where
  toOutput :: o -> Builder

instance Output [Value] where
  toOutput = (<> "\n") . fromEncoding . toEncoding

instance Output ByteString where
  toOutput = byteString

instance Output Builder where
  toOutput = id
