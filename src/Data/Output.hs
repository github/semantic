module Data.Output where

import Prologue

class Monoid o => Output o where
  toOutput :: o -> ByteString

instance Output ByteString where
  toOutput s = s
