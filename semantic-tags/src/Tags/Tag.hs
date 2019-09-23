module Tags.Tag
( Tag(..)
, Kind(..)
) where

import Data.Text (Text)
import Source.Span

data Tag = Tag
  { name :: Text
  , kind :: Kind
  , span :: Span
  , line :: Text
  , docs :: Maybe Text
  }
  deriving (Eq, Show)

data Kind
  = Function
  | Method
  | Class
  | Module
  | Call
  deriving (Bounded, Enum, Eq, Show)
