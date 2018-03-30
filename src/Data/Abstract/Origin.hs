module Data.Abstract.Origin where

import Data.Abstract.Module
import Data.Range
import Data.Span
import Prologue

data Origin
  = Unknown
  | Local !ModuleName !FilePath !Range !Span
  deriving (Eq, Ord, Show)

instance Semigroup Origin where
  a       <> Unknown = a
  _       <> b       = b

instance Monoid Origin where
  mempty = Unknown
  mappend = (<>)
