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
  | Type
  -- Just as Call is to Class and Function, Implements is to Interface
  | Interface
  | Implementation
  -- Constant -- TODO: New kind for constant references
  deriving (Bounded, Enum, Eq, Show)
