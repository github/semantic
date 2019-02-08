{-# LANGUAGE DeriveAnyClass #-}

module Data.Tag
  ( Tag (..)
  ) where

import Prologue

import Data.Aeson

import Data.Span

data Tag = Tag
  { name :: Text
  , kind :: Text
  , span :: Span
  , context :: [Text]
  , line :: Maybe Text
  , docs :: Maybe Text
  } deriving (Eq, Show, Generic, ToJSON)
