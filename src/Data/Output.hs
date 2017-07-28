module Data.Output where

import Prologue

class Monoid o => Output o where
  toOutput :: o -> ByteString
