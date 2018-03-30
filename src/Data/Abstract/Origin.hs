module Data.Abstract.Origin where

import Data.Abstract.Module
import Data.Range
import Data.Span

data Origin
  = Unknown
  | Local ModuleName FilePath Range Span
