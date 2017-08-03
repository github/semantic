module Data.Output where

import Data.ByteString (ByteString)

class Monoid o => Output o where
  toOutput :: o -> ByteString

instance Output ByteString where
  toOutput s = s
