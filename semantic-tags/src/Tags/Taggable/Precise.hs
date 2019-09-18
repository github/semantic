{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Tags.Taggable.Precise
( Python(..)
) where

import           Data.Aeson as A
import           Data.Span
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified TreeSitter.Python.AST as Python

data Tag = Tag
  { name :: Text
  , kind :: Text
  , span :: Span
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tag


newtype Python a = Python { getPython :: Python.Module a }
