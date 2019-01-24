{-# LANGUAGE DerivingVia, DerivingStrategies, DeriveAnyClass, DuplicateRecordFields #-}
module Semantic.API.LegacyTypes
  ( ParseTreeSymbolResponse(..)
  , File(..)
  , Symbol(..)
  , Span(..)
  , Position(..)
  ) where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics
import           Proto3.Suite as Proto3

--
-- Legacy Symbols API
--
newtype ParseTreeSymbolResponse = ParseTreeSymbolResponse { files :: [File] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data File = File
  { filePath :: T.Text
  , fileLanguage :: T.Text
  , fileSymbols :: [Symbol]
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Named, Message)

instance ToJSON File where
  toJSON File{..}
    = object [ "path"     .= filePath
             , "language" .= fileLanguage
             , "symbols"  .= fileSymbols
             ]

data Symbol = Symbol
  { symbolName :: T.Text
  , symbolKind :: T.Text
  , symbolLine :: T.Text
  , symbolSpan :: Maybe Span
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Named, Message)

instance ToJSON Symbol where
  toJSON Symbol{..}
    = object [ "symbol" .= symbolName
             , "kind"   .= symbolKind
             , "line"   .= symbolLine
             , "span"   .= symbolSpan
             ]

data Position = Position { line :: Int, column :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named)

instance ToJSON Position
  where toJSON Position{..} = toJSON [line, column]

data Span = Span { start :: Maybe Position, end :: Maybe Position }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)
