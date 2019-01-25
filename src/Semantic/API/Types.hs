{-# LANGUAGE DerivingVia, DerivingStrategies, DeriveAnyClass, DuplicateRecordFields, LambdaCase #-}
module Semantic.API.Types
  (
  -- Parse APIs
    ParseTreeRequest(..)

  -- Symbols for jump-to-definition
  , ParseTreeSymbolResponse(..)
  , File(..)
  , Symbol(..)

  -- Diff APIs
  , DiffTreeRequest(..)

  -- TOC Summaries
  , DiffTreeTOCResponse(..)
  , TOCSummaryFile(..)
  , TOCSummaryChange(..)
  , TOCSummaryError(..)
  , ChangeType(..)

  -- Diff tree graphs
  , DiffTreeGraphResponse(..)
  , DiffTreeEdge(..)
  , DiffTreeVertex(..)
  , DiffTreeTerm(..)
  , DeletedTerm(..)
  , InsertedTerm(..)
  , ReplacedTerm(..)
  , MergedTerm(..)

  -- Parse tree graphs
  , ParseTreeGraphResponse(..)
  , TermVertex(..)
  , TermEdge(..)
  , TermError(..)

  -- Health Check
  , PingRequest(..)
  , PingResponse(..)

  -- Common Types
  , Span(..)
  , Position(..)

  -- Mime Types
  , Protobuf
  ) where

import           Data.Aeson
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Char (toUpper)
import           Data.Graph (VertexTag (..))
import           Data.String
import qualified Data.Text as T
import           GHC.Generics
import           Network.HTTP.Media ((//))
import           Prologue
import           Proto3.Suite as Proto3
import           Servant.API

-- TODO: Remove dependence on these:
import Data.Blob
import Data.Language

-- These types represent the public API of semantic and are used to generate
-- `proto/semantic.proto`.
--
-- Some guidelines:
--
--   * Don't write Message, Named, ToJSON, or FromJSON instances by hand, derive
--     them.
--
--   * For non-primative types, you'll always want to use Maybe as protobuf
--     fields are always optional.
--
--   * It's usually best to map internal types to these API types so that the
--     API contract can be changed intentionally. This also makes it so that core
--     functionality doesn't have to deal with all the Maybes.
--
--   * Keep field names short and meaningful for external consumers. It's better
--     to skirt Haskell naming conventions in favor of consistency in our proto
--     files.

--
-- Parse/Term APIs
--

newtype ParseTreeRequest = ParseTreeRequest { blobs :: [Blob] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, FromJSON)

--
-- Symbols API
--
newtype ParseTreeSymbolResponse = ParseTreeSymbolResponse { files :: [File] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data File
  = File
  { path :: T.Text
  , language :: Language
  , symbols :: [Symbol]
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Named, Message, ToJSON)

data Symbol
  = Symbol
  { symbol :: T.Text
  , kind :: T.Text
  , line :: T.Text
  , span :: Maybe Span
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Named, Message, ToJSON)

--
-- Term Graph API
--
data ParseTreeGraphResponse
  = ParseTreeGraphResponse
  { vertices :: [TermVertex]
  , edges :: [TermEdge]
  , errors :: [TermError]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)
  deriving Semigroup via GenericSemigroup ParseTreeGraphResponse
  deriving Monoid via GenericMonoid ParseTreeGraphResponse

data TermEdge = TermEdge { source :: Int, target :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data TermVertex = TermVertex { vertexId :: Int, term :: String, span :: Maybe Span }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)
instance VertexTag TermVertex where uniqueTag = vertexId

data TermError = TermError { path :: String, error :: String }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

--
-- Diff APIs
--

newtype DiffTreeRequest = DiffTreeRequest { blobs :: [BlobPair] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, FromJSON)

--
-- TOC Summaries API
--
newtype DiffTreeTOCResponse = DiffTreeTOCResponse { files :: [TOCSummaryFile] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data TOCSummaryFile = TOCSummaryFile
  { path :: T.Text
  , language :: Language
  , changes :: [TOCSummaryChange]
  , errors  :: [TOCSummaryError]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data TOCSummaryChange = TOCSummaryChange
  { category :: T.Text
  , term :: T.Text
  , span :: Maybe Span
  , changeType :: ChangeType
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data ChangeType
  = None
  | Added
  | Removed
  | Modified
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Named, MessageField, ToJSON)

instance HasDefault ChangeType where def = None

instance Finite ChangeType where
  enumerate _ = fmap go [None ..] where
    go x = (fromString (fmap toUpper (show x)), fromEnum x)

instance Primitive ChangeType where
  primType _ = primType (Proxy @(Enumerated ChangeType))
  encodePrimitive f = encodePrimitive f . Enumerated . Right
  decodePrimitive   = decodePrimitive >>= \case
    (Enumerated (Right r)) -> pure r
    other                  -> Prelude.fail ("ChangeType decodeMessageField: unexpected value" <> show other)

data TOCSummaryError = TOCSummaryError
  { error :: T.Text
  , span :: Maybe Span
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

--
-- Diff Tree Graph API
--

data DiffTreeGraphResponse
  = DiffTreeGraphResponse { vertices :: [DiffTreeVertex], edges :: [DiffTreeEdge] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)
  deriving Semigroup via GenericSemigroup DiffTreeGraphResponse
  deriving Monoid via GenericMonoid DiffTreeGraphResponse

data DiffTreeEdge = DiffTreeEdge { source :: Int, target :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data DiffTreeVertex = DiffTreeVertex { diffVertexId :: Int, term :: Maybe DiffTreeTerm }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)
instance VertexTag DiffTreeVertex where uniqueTag = diffVertexId

-- NB: Current proto generation only supports sum types with single named fields.
data DiffTreeTerm
  = Deleted  { deleted  :: Maybe DeletedTerm }
  | Inserted { inserted :: Maybe InsertedTerm }
  | Replaced { replaced :: Maybe ReplacedTerm }
  | Merged   { merged   :: Maybe MergedTerm }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data DeletedTerm = DeletedTerm { term :: String, span :: Maybe Span }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data InsertedTerm = InsertedTerm { term :: String, span :: Maybe Span }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data ReplacedTerm = ReplacedTerm { beforeTerm :: String, beforeSpan :: Maybe Span, afterTerm :: String, afterSpan :: Maybe Span }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

data MergedTerm = MergedTerm { term :: String, beforeSpan :: Maybe Span, afterSpan :: Maybe Span }
  deriving stock (Eq, Ord, Show, Generic)
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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

instance MimeRender PlainText PingResponse where
  mimeRender _ PingResponse{..} = BC.pack $
    status <> " - " <> hostname <> " - " <> sha <> " - " <> timestamp <> "\n"


--
-- Common Types
--

data Position = Position
  { line :: Int
  , column :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Message, Named, ToJSON)

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
