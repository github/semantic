{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Proto.Semantic_JSON where

import           Control.Lens hiding ((.=))
import           Control.Monad (msum)
import           Data.Aeson as A
import qualified Data.Aeson.Encoding as E
import           Data.Aeson.Types (parseField)
import           Data.ProtoLens (defMessage)
import qualified Data.Text as T
import           Prelude hiding (error, span)
import           Proto.Semantic as P
import           Proto.Semantic_Fields as P


instance FromJSON PingRequest where
  parseJSON = withObject "PingRequest" $ \obj -> do
    service <- obj .: "service"
    pure $ defMessage & P.service .~ service

instance ToJSON PingRequest where
  toJSON x = object [ "service" .= (x^.service) ]
  toEncoding x = pairs $ "service" .= (x^.service)

instance FromJSON PingResponse where
  parseJSON = A.withObject "PingResponse" $ \obj -> do
    status <- obj .: "status"
    hostname <- obj .: "hostname"
    timestamp <- obj .: "timestamp"
    sha <- obj .: "sha"
    pure $ defMessage
      & P.status .~ status
      & P.hostname .~ hostname
      & P.timestamp .~ timestamp
      & P.sha .~ sha

instance ToJSON PingResponse where
  toJSON x = object
    [ "status" .= (x^.status)
    , "hostname" .= (x^.hostname)
    , "timestamp" .= (x^.timestamp)
    , "sha" .= (x^.sha)
    ]
  toEncoding x = pairs $
       "status" .= (x^.status)
    <> "hostname" .= (x^.hostname)
    <> "timestamp" .= (x^.timestamp)
    <> "sha" .= (x^.sha)

instance FromJSON ParseTreeRequest where
  parseJSON = withObject "ParseTreeRequest" $ \obj -> do
    blobs <- obj .: "blobs"
    pure $ defMessage & P.blobs .~ blobs

instance ToJSON ParseTreeRequest where
  toJSON x = object [ "blobs" .= (x^.blobs) ]
  toEncoding x = pairs $ "blobs" .= (x^.blobs)

instance FromJSON Blob where
  parseJSON = withObject "Blob" $ \obj -> do
    content <- obj .: "content"
    path <- obj .: "path"
    language <- obj .: "language"
    pure $ defMessage
      & P.content .~ content
      & P.path .~ path
      & P.language .~ language

instance ToJSON Blob where
  toJSON x = object
    [ "content" .= (x^.content)
    , "path" .= (x^.path)
    , "language" .= (x^.language)
    ]
  toEncoding x = pairs $
      "content" .= (x^.content)
    <> "path" .= (x^.path)
    <> "language" .= (x^.language)

instance FromJSON ParseTreeSymbolResponse where
  parseJSON = withObject "ParseTreeSymbolResponse" $ \obj -> do
    files <- obj .: "files"
    pure $ defMessage & P.files .~ files

instance ToJSON ParseTreeSymbolResponse where
  toJSON x = object [ "files" .= (x^.files) ]
  toEncoding x = pairs $ "files" .= (x^.files)

instance FromJSON ParseTreeGraphResponse where
  parseJSON = withObject "ParseTreeGraphResponse" $ \obj -> do
    files <- obj .: "files"
    pure $ defMessage & P.files .~ files

instance ToJSON ParseTreeGraphResponse where
  toJSON x = object [ "files" .= (x^.files) ]
  toEncoding x = pairs $ "files" .= (x^.files)

instance FromJSON ParseTreeFileGraph where
  parseJSON = withObject "ParseTreeFileGraph" $ \obj -> do
    path <- obj .: "path"
    language <- obj .: "language"
    vertices <- obj .: "vertices"
    edges <- obj .: "edges"
    errors <- obj .: "errors"
    pure $ defMessage
      & P.path .~ path
      & P.language .~ language
      & P.vertices .~ vertices
      & P.edges .~ edges
      & P.errors .~ errors

instance ToJSON ParseTreeFileGraph where
  toJSON x = object
    [ "path" .= (x^.path)
    , "language" .= (x^.language)
    , "vertices" .= (x^.vertices)
    , "edges" .= (x^.edges)
    , "errors" .= (x^.errors)
    ]
  toEncoding x = pairs $
       "path" .= (x^.path)
    <> "language" .= (x^.language)
    <> "vertices" .= (x^.vertices)
    <> "edges" .= (x^.edges)
    <> "errors" .= (x^.errors)

instance FromJSON TermEdge where
  parseJSON = withObject "TermEdge" $ \obj -> do
    source <- obj .: "source"
    target <- obj .: "target"
    pure $ defMessage & P.source .~ source & P.target .~ target

instance ToJSON TermEdge where
  toJSON x = object [ "source" .= (x^.source), "target" .= (x^.target) ]
  toEncoding x = pairs $ "source" .= (x^.source) <> "target" .= (x^.target)

instance FromJSON TermVertex where
  parseJSON = withObject "TermVertex" $ \obj -> do
    vertexId <- obj .: "vertexId"
    term <- obj .: "term"
    span <- obj .: "span"
    pure $ defMessage
      & P.vertexId .~ vertexId
      & P.term .~ term
      & P.span .~ span

instance ToJSON TermVertex where
  toJSON x = object
    [ "vertexId" .= (x^.vertexId)
    , "term" .= (x^.term)
    , "span" .= (x^.span)
    ]
  toEncoding x = pairs $
       "vertexId" .= (x^.vertexId)
    <> "term" .= (x^.term)
    <> "span" .= (x^.span)

instance FromJSON File where
  parseJSON = withObject "File" $ \obj -> do
    path <- obj .: "path"
    language <- obj .: "language"
    symbols <- obj .: "symbols"
    errors <- obj .: "errors"
    blobOid <- obj .: "blobOid"
    pure $ defMessage
      & P.path .~ path
      & P.language .~ language
      & P.symbols .~ symbols
      & P.errors .~ errors
      & P.blobOid .~ blobOid

instance ToJSON File where
  toJSON x = object
    [ "path" .= (x^.path)
    , "language" .= (x^.language)
    , "symbols" .= (x^.symbols)
    , "errors" .= (x^.errors)
    , "blobOid" .= (x^.blobOid)
    ]
  toEncoding x = pairs $
       "path" .= (x^.path)
    <> "language" .= (x^.language)
    <> "symbols" .= (x^.symbols)
    <> "errors" .= (x^.errors)
    <> "blobOid" .= (x^.blobOid)

instance FromJSON Symbol where
  parseJSON = withObject "Symbol" $ \obj -> do
    symbol <- obj .: "symbol"
    kind <- obj .: "kind"
    line <- obj .: "line"
    span <- obj .: "span"
    docs <- obj .: "docs"
    pure $ defMessage
      & P.symbol .~ symbol
      & P.kind .~ kind
      & P.line .~ line
      & P.span .~ span
      & P.docs .~ docs

instance ToJSON Symbol where
  toJSON x = object
    [ "symbol" .= (x^.symbol)
    , "kind" .= (x^.kind)
    , "line" .= (x^.line)
    , "span" .= (x^.span)
    , "docs" .= (x^.docs)
    ]
  toEncoding x = pairs $
       "symbol" .= (x^.symbol)
    <> "kind" .= (x^.kind)
    <> "line" .= (x^.line)
    <> "span" .= (x^.span)
    <> "docs" .= (x^.docs)

instance FromJSON Span where
  parseJSON = withObject "Span" $ \obj -> do
    start <- obj .: "start"
    end <- obj .: "end"
    pure $ defMessage & P.start .~ start & P.end .~ end

instance ToJSON Span where
  toJSON x = object [ "start" .= (x^.start) , "end" .= (x^.end) ]
  toEncoding x = pairs $ "start" .= (x^.start) <> "end" .= (x^.end)

instance FromJSON Position where
  parseJSON = withObject "Position" $ \obj -> do
    line <- obj .: "line"
    column <- obj .: "column"
    pure $ defMessage & P.line .~ line & P.column .~ column

instance ToJSON Position where
  toJSON x = object [ "line" .= (x^.line) , "column" .= (x^.column) ]
  toEncoding x = pairs $ "line" .= (x^.line) <> "column" .= (x^.column)

instance FromJSON Docstring where
  parseJSON = withObject "Docstring" $ \obj -> do
    docstring <- obj .: "docstring"
    pure $ defMessage & P.docstring .~ docstring

instance ToJSON Docstring where
  toJSON x = object [ "docstring" .= (x^.docstring) ]
  toEncoding x = pairs $ "docstring" .= (x^.docstring)

instance FromJSON ParseError where
  parseJSON = withObject "ParseError" $ \obj -> do
    error <- obj .: "error"
    pure $ defMessage & P.error .~ error

instance ToJSON ParseError where
  toJSON x = object [ "error" .= (x^.error) ]
  toEncoding x =  pairs $ "error" .= (x^.error)

instance FromJSON DiffTreeRequest where
  parseJSON = withObject "DiffTreeRequest" $ \obj -> do
    blobs <- obj .: "blobs"
    pure $ defMessage & P.blobs .~ blobs

instance ToJSON DiffTreeRequest where
  toJSON x = object [ "blobs" .= (x^.blobs) ]
  toEncoding x = pairs $ "blobs" .= (x^.blobs)

instance FromJSON BlobPair where
  parseJSON = withObject "BlobPair" $ \obj -> do
    before <- obj .: "before"
    after <- obj .: "after"
    pure $ defMessage & P.before .~ before & P.after .~ after

instance ToJSON BlobPair where
  toJSON x = object [ "before" .= (x^.before), "after" .= (x^.after) ]
  toEncoding x = pairs $ "before" .= (x^.before) <> "after" .= (x^.after)

instance FromJSON DiffTreeTOCResponse where
  parseJSON = withObject "DiffTreeTOCResponse" $ \obj -> do
    files <- obj .: "files"
    pure $ defMessage & P.files .~ files

instance ToJSON DiffTreeTOCResponse where
  toJSON x = object [ "files" .= (x^.files) ]
  toEncoding x = pairs $ "files" .= (x^.files)

instance FromJSON TOCSummaryFile where
  parseJSON = withObject "TOCSummaryFile" $ \obj -> do
    path <- obj .: "path"
    language <- obj .: "language"
    changes <- obj .: "changes"
    errors <- obj .: "errors"
    pure $ defMessage
      & P.path .~ path
      & P.language .~ language
      & P.changes .~ changes
      & P.errors .~ errors

instance ToJSON TOCSummaryFile where
  toJSON x = object
    [ "path" .= (x^.path)
    , "language" .= (x^.language)
    , "changes" .= (x^.changes)
    , "errors" .= (x^.errors)
    ]
  toEncoding x = pairs $
       "path" .= (x^.path)
    <> "language" .= (x^.language)
    <> "changes" .= (x^.changes)
    <> "errors" .= (x^.errors)

instance FromJSON TOCSummaryChange where
  parseJSON = withObject "TOCSummaryChange" $ \obj -> do
    category <- obj .: "category"
    term <- obj .: "term"
    span <- obj .: "span"
    changeType <- obj .: "changeType"
    pure $ defMessage
      & P.category .~ category
      & P.term .~ term
      & P.span .~ span
      & P.changeType .~ changeType

instance ToJSON TOCSummaryChange where
  toJSON x = object
    [ "category" .= (x^.category)
    , "term" .= (x^.term)
    , "span" .= (x^.span)
    , "changeType" .= (x^.changeType)
    ]
  toEncoding x = pairs $
      "category" .= (x^.category)
    <> "term" .= (x^.term)
    <> "span" .= (x^.span)
    <> "changeType" .= (x^.changeType)

instance FromJSON TOCSummaryError where
  parseJSON = withObject "TOCSummaryError" $ \obj -> do
    error <- obj .: "error"
    span <- obj .: "span"
    pure $ defMessage & P.error .~ error & P.span .~ span

instance ToJSON TOCSummaryError where
  toJSON x = object [ "error" .= (x^.error), "span" .= (x^.span) ]
  toEncoding x = pairs $ "error" .= (x^.error) <> "span" .= (x^.span)

instance FromJSON ChangeType where
  parseJSON (A.String "NONE") = pure NONE
  parseJSON (A.String "ADDED") = pure ADDED
  parseJSON (A.String "REMOVED") = pure REMOVED
  parseJSON (A.String "MODIFIED") = pure MODIFIED
  parseJSON _ = fail "unexpected ChangeType"

instance ToJSON ChangeType where
  toJSON x = A.String . T.toUpper . T.pack $ show x
  toEncoding x = E.text . T.toUpper . T.pack  $ show x

instance FromJSON DiffTreeGraphResponse where
  parseJSON = withObject "DiffTreeGraphResponse" $ \obj -> do
    files <- obj .: "files"
    pure $ defMessage & P.files .~ files

instance ToJSON DiffTreeGraphResponse where
  toJSON x = object [ "files" .= (x^.files) ]
  toEncoding x = pairs $ "files" .= (x^.files)

instance FromJSON DiffTreeFileGraph where
  parseJSON = withObject "DiffTreeFileGraph" $ \obj -> do
    path <- obj .: "path"
    language <- obj .: "language"
    vertices <- obj .: "vertices"
    edges <- obj .: "edges"
    errors <- obj .: "errors"
    pure $ defMessage
      & P.path .~ path
      & P.language .~ language
      & P.vertices .~ vertices
      & P.edges .~ edges
      & P.errors .~ errors

instance ToJSON DiffTreeFileGraph where
  toJSON x = object
    [ "path" .= (x^.path)
    , "language" .= (x^.language)
    , "vertices" .= (x^.vertices)
    , "edges" .= (x^.edges)
    , "errors" .= (x^.errors)
    ]
  toEncoding x = pairs $
       "path" .= (x^.path)
    <> "language" .= (x^.language)
    <> "vertices" .= (x^.vertices)
    <> "edges" .= (x^.edges)
    <> "errors" .= (x^.errors)

instance FromJSON DiffTreeEdge where
  parseJSON = withObject "DiffTreeEdge" $ \obj -> do
    source <- obj .: "source"
    target <- obj .: "target"
    pure $ defMessage & P.source .~ source & P.target .~ target

instance ToJSON DiffTreeEdge where
  toJSON x = object [ "source" .= (x^.source), "target" .= (x^.target) ]
  toEncoding x = pairs $ "source" .= (x^.source) <> "target" .= (x^.target)

instance FromJSON DiffTreeVertex where
  parseJSON = withObject "DiffTreeVertex" $ \obj -> do
    diffVertexId <- obj .: "diffVertexId"
    diffTerm <- obj .: "diffTerm"
    pure $ defMessage
      & P.diffVertexId .~ diffVertexId
      & P.maybe'diffTerm .~ diffTerm

instance ToJSON DiffTreeVertex where
  toJSON x = object
    [ "diffVertexId" .= (x^.diffVertexId)
    , "diffTerm" .= (x^.maybe'diffTerm)
    ]
  toEncoding x = pairs $
       "diffVertexId" .= (x^.diffVertexId)
    <> "diffTerm" .= (x^.maybe'diffTerm)

instance FromJSON DiffTreeVertex'DiffTerm where
  parseJSON = withObject "DiffTreeVertexDiffTerm" $ \obj -> msum
    [
      DiffTreeVertex'Deleted <$> parseField obj "deleted"
    , DiffTreeVertex'Inserted <$> parseField obj "inserted"
    , DiffTreeVertex'Replaced <$> parseField obj "replaced"
    , DiffTreeVertex'Merged <$> parseField obj "merged"
    ]

instance ToJSON DiffTreeVertex'DiffTerm where
  toJSON (DiffTreeVertex'Deleted x) = object [ "deleted" .= x ]
  toJSON (DiffTreeVertex'Inserted x) = object [ "inserted" .= x ]
  toJSON (DiffTreeVertex'Replaced x) = object [ "replaced" .= x ]
  toJSON (DiffTreeVertex'Merged x) = object [ "merged" .= x ]
  toEncoding (DiffTreeVertex'Deleted x) = pairs $ "deleted" .= x
  toEncoding (DiffTreeVertex'Inserted x) = pairs $ "inserted" .= x
  toEncoding (DiffTreeVertex'Replaced x) = pairs $ "replaced" .= x
  toEncoding (DiffTreeVertex'Merged x) = pairs $ "merged" .= x

instance FromJSON MergedTerm where
  parseJSON = withObject "MergedTerm" $ \obj -> do
    term <- obj .: "term"
    beforeSpan <- obj .: "beforeSpan"
    afterSpan <- obj .: "afterSpan"
    pure $ defMessage
      & P.term .~ term
      & P.beforeSpan .~ beforeSpan
      & P.afterSpan .~ afterSpan

instance ToJSON MergedTerm where
  toJSON x = object
    [ "term" .= (x^.term)
    , "beforeSpan" .= (x^.beforeSpan)
    , "afterSpan" .= (x^.afterSpan)
    ]
  toEncoding x = pairs $
       "term" .= (x^.term)
    <> "beforeSpan" .= (x^.beforeSpan)
    <> "afterSpan" .= (x^.afterSpan)

instance FromJSON ReplacedTerm where
  parseJSON = withObject "ReplacedTerm" $ \obj -> do
    beforeTerm <- obj .: "beforeTerm"
    beforeSpan <- obj .: "beforeSpan"
    afterTerm <- obj .: "afterTerm"
    afterSpan <- obj .: "afterSpan"
    pure $ defMessage
      & P.beforeTerm .~ beforeTerm
      & P.beforeSpan .~ beforeSpan
      & P.afterTerm .~ afterTerm
      & P.afterSpan .~ afterSpan

instance ToJSON ReplacedTerm where
  toJSON x = object
    [ "beforeTerm" .= (x^.beforeTerm)
    , "beforeSpan" .= (x^.beforeSpan)
    , "afterTerm" .= (x^.afterTerm)
    , "afterSpan" .= (x^.afterSpan)
    ]
  toEncoding x = pairs $
       "beforeTerm" .= (x^.beforeTerm)
    <> "beforeSpan" .= (x^.beforeSpan)
    <> "afterTerm" .= (x^.afterTerm)
    <> "afterSpan" .= (x^.afterSpan)

instance FromJSON InsertedTerm where
  parseJSON = withObject "InsertedTerm" $ \obj -> do
    term <- obj .: "term"
    span <- obj .: "span"
    pure $ defMessage & P.term .~ term & P.span .~ span

instance ToJSON InsertedTerm where
  toJSON x = object [ "term" .= (x^.term), "span" .= (x^.span) ]
  toEncoding x = pairs $ "term" .= (x^.term) <> "span" .= (x^.span)

instance FromJSON DeletedTerm where
  parseJSON = withObject "DeletedTerm" $ \obj -> do
    term <- obj .: "term"
    span <- obj .: "span"
    pure $ defMessage & P.term .~ term & P.span .~ span

instance ToJSON DeletedTerm where
  toJSON x = object [ "term" .= (x^.term), "span" .= (x^.span) ]
  toEncoding x = pairs $ "term" .= (x^.term) <> "span" .= (x^.span)
