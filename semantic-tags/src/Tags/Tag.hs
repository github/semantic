module Tags.Tag
( Tag(..)
, Kind(..)
) where

import Data.Text (Text)
import Source.Loc

data Tag = Tag
  { name :: Text
  , kind :: Kind
  , loc  :: Loc
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
