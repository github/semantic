{-# LANGUAGE DerivingVia, DeriveAnyClass, DuplicateRecordFields #-}
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

--
-- Legacy Symbols API
--

newtype ParseTreeRequest = ParseTreeRequest { blobs :: [Blob] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype ParseTreeSymbolResponse = ParseTreeSymbolResponse { files :: [File] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data File = File
  { filePath :: Text
  , fileLanguage :: Text
  , fileSymbols :: [Symbol]
  }
  deriving stock (Generic, Eq, Show)

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
  deriving stock (Generic, Eq, Show)

instance ToJSON Symbol where
  toJSON Symbol{..}
    = object [ "symbol" .= symbolName
             , "kind"   .= symbolKind
             , "line"   .= symbolLine
             , "span"   .= symbolSpan
             ]

data Position = Position { line :: Int, column :: Int }
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON Position
  where toJSON Position{..} = toJSON [line, column]

data Span = Span { start :: Maybe Position, end :: Maybe Position }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)
