{-# LANGUAGE DeriveGeneric #-}
module Data.Language
( Language (..)
) where

import Data.Aeson
import GHC.Generics (Generic)

-- | The various languages we support.
-- Please do not reorder any of the field names: the current implementation of 'Primitive'
-- delegates to the auto-generated 'Enum' instance.
data Language
  = Unknown
  | Go
  | Haskell
  | Java
  | JavaScript
  | JSON
  | JSX
  | Markdown
  | Python
  | Ruby
  | TypeScript
  | PHP
  | TSX
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance ToJSON Language
