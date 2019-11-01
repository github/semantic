{-# LANGUAGE DeriveGeneric, DerivingVia, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}
module Semantic.Api.LegacyTypes
  ( DiffTreeRequest(..)
  , ParseTreeRequest(..)
  , ParseTreeSymbolResponse(..)
  , File(..)
  , Symbol(..)
  , Span(..)
  , Position(..)
  ) where

import Data.Aeson
import Data.Blob hiding (File(..))
import Prologue

newtype DiffTreeRequest = DiffTreeRequest { blobs :: [BlobPair] }
  deriving (Eq, Show, Generic, FromJSON)

--
-- Legacy Symbols API
--

newtype ParseTreeRequest = ParseTreeRequest { blobs :: [Blob] }
  deriving (Eq, Show, Generic, FromJSON)

newtype ParseTreeSymbolResponse = ParseTreeSymbolResponse { files :: [File] }
  deriving (Eq, Show, Generic, ToJSON)

data File = File
  { filePath :: Text
  , fileLanguage :: Text
  , fileSymbols :: [Symbol]
  }
  deriving (Eq, Show, Generic)

instance ToJSON File where
  toJSON File{..}
    = object [ "path"     .= filePath
             , "language" .= fileLanguage
             , "symbols"  .= fileSymbols
             ]

data Symbol = Symbol
  { symbolName :: Text
  , symbolKind :: Text
  , symbolLine :: Text
  , symbolSpan :: Maybe Span
  }
  deriving (Generic, Eq, Show)

instance ToJSON Symbol where
  toJSON Symbol{..}
    = object [ "symbol" .= symbolName
             , "kind"   .= symbolKind
             , "line"   .= symbolLine
             , "span"   .= symbolSpan
             ]

data Position = Position { line :: Int, column :: Int }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Position
  where toJSON Position{..} = toJSON [line, column]

data Span = Span { start :: Maybe Position, end :: Maybe Position }
  deriving (Eq, Ord, Show, Generic, ToJSON)
