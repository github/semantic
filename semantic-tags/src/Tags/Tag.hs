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
  -- Definitions
  = Function
  | Method
  | Class
  | Module
  -- References
  | Call
  -- Constant -- TODO: New kind for constant references
  deriving (Bounded, Enum, Eq, Show)
