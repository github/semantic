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
  -- Just as Call is to Class and Function, Implementation is to Interface.
  -- This suggests that perhaps we should have an Instantiation kind that
  -- we use for Class.
  | Interface
  | Implementation
  -- Constant -- TODO: New kind for constant references
  deriving (Bounded, Enum, Eq, Show)
