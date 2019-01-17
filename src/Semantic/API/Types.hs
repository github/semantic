{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DuplicateRecordFields #-}
module Semantic.API.Types
  (
  -- Symbols for jump-to-definition
    ParseTreeRequest(..)
  , ParseTreeSymbolResponse(..)
  , File(..)
  , Symbol(..)

  -- TOC Summaries
  , DiffTreeRequest(..)
  , DiffTreeTOCResponse(..)
  , TOCSummaryChange(..)
  , TOCSummaryError(..)

  -- Health Check
  , PingRequest(..)
  , PingResponse(..)

  -- Types
  , Span(..)
  , Position(..)

  -- Mime Types
  , Protobuf
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import           GHC.Generics
import           Proto3.Suite as Proto3
import Network.HTTP.Media ((//))
import           Servant.API
import Data.Bifunctor (first)

-- TODO: Remove dependence on these:
import Data.Blob

--
-- Symbols API
--

newtype ParseTreeRequest = ParseTreeRequest { blobs :: [Blob] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, FromJSON)

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

-- TODO: Can remove custom ToJSON once clients migrate to twirp/proto.
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

-- TODO: Can remove custom ToJSON once clients migrate to twirp/proto.
instance ToJSON Symbol where
  toJSON Symbol{..}
    = object [ "symbol" .= symbolName
             , "kind"   .= symbolKind
             , "line"   .= symbolLine
             , "span"   .= symbolSpan
             ]


--
-- TOC Summaries API
--

newtype DiffTreeRequest = DiffTreeRequest { blobs :: [BlobPair] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, FromJSON)

data DiffTreeTOCResponse = DiffTreeTOCResponse
  { changes :: [TOCSummaryChange]
  , errors  :: [TOCSummaryError]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

instance Semigroup DiffTreeTOCResponse where
  (DiffTreeTOCResponse c1 e1) <> (DiffTreeTOCResponse c2 e2) = DiffTreeTOCResponse (c1 <> c2) (e1 <> e2)

instance Monoid DiffTreeTOCResponse where
  mempty = DiffTreeTOCResponse mempty mempty

data TOCSummaryChange = TOCSummaryChange
  { category :: T.Text
  , term :: T.Text
  , span :: Maybe Span
  , changeType :: T.Text -- TODO: could be enum
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data TOCSummaryError = TOCSummaryError
  { error :: T.Text
  , span :: Maybe Span
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)


--
-- Health Check API
--

newtype PingRequest = PingRequest { service :: String }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, FromJSON)

data PingResponse
  = PingResponse
  { status :: String
  , hostname :: String
  , timestamp :: String
  , sha :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

instance MimeRender PlainText PingResponse where
  mimeRender _ PingResponse{..} = BC.pack $
    status <> " - " <> hostname <> " - " <> sha <> " - " <> timestamp <> "\n"


--
-- Common Types
--

-- TODO: Custom Types for Blob, Source, etc.

-- data Language
--     = Unknown
--     | Go
--     | Haskell
--     | Java
--     | JavaScript
--     | JSON
--     | JSX
--     | Markdown
--     | Python
--     | Ruby
--     | TypeScript
--     | PHP
--   deriving stock (Eq, Ord, Show, Generic)
--   deriving anyclass (Named, Enum, MessageField, ToJSON)
--
-- -- This ensures that the protobuf file is generated with ALL_CAPS_NAMES.
-- instance Finite Language where
--   enumerate _ = fmap go [Unknown ..] where
--     go x = (fromString (fmap toUpper (show x)), fromEnum x)
--
-- instance FromJSON Language where
--   parseJSON = withText "Language" $ \l -> pure $ case T.toLower l of
--     "go"         -> Go
--     "haskell"    -> Haskell
--     "java"       -> Java
--     "javascript" -> JavaScript
--     "json"       -> JSON
--     "jsx"        -> JSX
--     "markdown"   -> Markdown
--     "python"     -> Python
--     "ruby"       -> Ruby
--     "typescript" -> TypeScript
--     "php"        -> PHP
--     _            -> Unknown


data Position = Position
  { line :: Int
  , column :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named)

-- TODO: Can remove custom ToJSON once clients migrate to twirp/proto.
instance ToJSON Position where toJSON Position{..} = toJSON [line, column]
instance FromJSON Position where
  parseJSON arr = do
    [line, col] <- parseJSON arr
    pure $ Position line col

data Span = Span
  { start :: Maybe Position
  , end :: Maybe Position
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

--
-- Custom Mime Types
--
-- Servant doesn't come with protobuf support out of the box, but it's
-- very easy to add this as a valid type for decoding and encoding: all
-- you have to do is map proto3-suite's encoding and decoding functions
-- to the MimeRender/MimeUnrender typeclasses.

data Protobuf

instance Accept Protobuf where
  contentType _ = "application" // "protobuf"

instance Message a => MimeRender Protobuf a where
  mimeRender _ = Proto3.toLazyByteString

instance Message a => MimeUnrender Protobuf a where
  mimeUnrender _ = first show . Proto3.fromByteString . BC.toStrict
