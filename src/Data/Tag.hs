{-# LANGUAGE DeriveAnyClass #-}

module Data.Tag
  ( Tag (..)
  ) where

import Prelude hiding (span)
import Prologue

import Data.Aeson
import Control.Lens.Lens

import Data.Span

data Tag = Tag
  { name :: Text
  , kind :: Text
  , tagSpan :: Span
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  } deriving (Eq, Show, Generic, ToJSON)

instance HasSpan Tag where
  span = lens tagSpan (\t s -> t { tagSpan = s })
  {-# INLINE span #-}
