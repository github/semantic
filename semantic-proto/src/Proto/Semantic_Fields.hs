{- This file was auto-generated from semantic.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Semantic_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
blobOid ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "blobOid" a) =>
  Lens.Family2.LensLike' f s a
blobOid = Data.ProtoLens.Field.field @"blobOid"
blobs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "blobs" a) =>
  Lens.Family2.LensLike' f s a
blobs = Data.ProtoLens.Field.field @"blobs"
byteRange ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "byteRange" a) =>
  Lens.Family2.LensLike' f s a
byteRange = Data.ProtoLens.Field.field @"byteRange"
column ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "column" a) =>
  Lens.Family2.LensLike' f s a
column = Data.ProtoLens.Field.field @"column"
content ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "content" a) =>
  Lens.Family2.LensLike' f s a
content = Data.ProtoLens.Field.field @"content"
docs ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "docs" a) =>
  Lens.Family2.LensLike' f s a
docs = Data.ProtoLens.Field.field @"docs"
docstring ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "docstring" a) =>
  Lens.Family2.LensLike' f s a
docstring = Data.ProtoLens.Field.field @"docstring"
edges ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "edges" a) =>
  Lens.Family2.LensLike' f s a
edges = Data.ProtoLens.Field.field @"edges"
end ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "end" a) =>
  Lens.Family2.LensLike' f s a
end = Data.ProtoLens.Field.field @"end"
endingScopeStack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "endingScopeStack" a) =>
  Lens.Family2.LensLike' f s a
endingScopeStack = Data.ProtoLens.Field.field @"endingScopeStack"
endingSymbolStack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "endingSymbolStack" a) =>
  Lens.Family2.LensLike' f s a
endingSymbolStack = Data.ProtoLens.Field.field @"endingSymbolStack"
error ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "error" a) =>
  Lens.Family2.LensLike' f s a
error = Data.ProtoLens.Field.field @"error"
errors ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "errors" a) =>
  Lens.Family2.LensLike' f s a
errors = Data.ProtoLens.Field.field @"errors"
files ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "files" a) =>
  Lens.Family2.LensLike' f s a
files = Data.ProtoLens.Field.field @"files"
from ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "from" a) =>
  Lens.Family2.LensLike' f s a
from = Data.ProtoLens.Field.field @"from"
hostname ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "hostname" a) =>
  Lens.Family2.LensLike' f s a
hostname = Data.ProtoLens.Field.field @"hostname"
id ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "id" a) =>
  Lens.Family2.LensLike' f s a
id = Data.ProtoLens.Field.field @"id"
kind ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "kind" a) =>
  Lens.Family2.LensLike' f s a
kind = Data.ProtoLens.Field.field @"kind"
language ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "language" a) =>
  Lens.Family2.LensLike' f s a
language = Data.ProtoLens.Field.field @"language"
line ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "line" a) =>
  Lens.Family2.LensLike' f s a
line = Data.ProtoLens.Field.field @"line"
maybe'byteRange ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'byteRange" a) =>
  Lens.Family2.LensLike' f s a
maybe'byteRange = Data.ProtoLens.Field.field @"maybe'byteRange"
maybe'docs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'docs" a) =>
  Lens.Family2.LensLike' f s a
maybe'docs = Data.ProtoLens.Field.field @"maybe'docs"
maybe'end ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'end" a) =>
  Lens.Family2.LensLike' f s a
maybe'end = Data.ProtoLens.Field.field @"maybe'end"
maybe'span ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'span" a) =>
  Lens.Family2.LensLike' f s a
maybe'span = Data.ProtoLens.Field.field @"maybe'span"
maybe'start ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'start" a) =>
  Lens.Family2.LensLike' f s a
maybe'start = Data.ProtoLens.Field.field @"maybe'start"
maybe'utf16CodeUnitSpan ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'utf16CodeUnitSpan" a) =>
  Lens.Family2.LensLike' f s a
maybe'utf16CodeUnitSpan
  = Data.ProtoLens.Field.field @"maybe'utf16CodeUnitSpan"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
nodeType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nodeType" a) =>
  Lens.Family2.LensLike' f s a
nodeType = Data.ProtoLens.Field.field @"nodeType"
nodes ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "nodes" a) =>
  Lens.Family2.LensLike' f s a
nodes = Data.ProtoLens.Field.field @"nodes"
path ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "path" a) =>
  Lens.Family2.LensLike' f s a
path = Data.ProtoLens.Field.field @"path"
paths ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "paths" a) =>
  Lens.Family2.LensLike' f s a
paths = Data.ProtoLens.Field.field @"paths"
service ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "service" a) =>
  Lens.Family2.LensLike' f s a
service = Data.ProtoLens.Field.field @"service"
sha ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "sha" a) =>
  Lens.Family2.LensLike' f s a
sha = Data.ProtoLens.Field.field @"sha"
span ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "span" a) =>
  Lens.Family2.LensLike' f s a
span = Data.ProtoLens.Field.field @"span"
start ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "start" a) =>
  Lens.Family2.LensLike' f s a
start = Data.ProtoLens.Field.field @"start"
startingScopeStackSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "startingScopeStackSize" a) =>
  Lens.Family2.LensLike' f s a
startingScopeStackSize
  = Data.ProtoLens.Field.field @"startingScopeStackSize"
startingSymbolStack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "startingSymbolStack" a) =>
  Lens.Family2.LensLike' f s a
startingSymbolStack
  = Data.ProtoLens.Field.field @"startingSymbolStack"
status ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "status" a) =>
  Lens.Family2.LensLike' f s a
status = Data.ProtoLens.Field.field @"status"
symbol ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "symbol" a) =>
  Lens.Family2.LensLike' f s a
symbol = Data.ProtoLens.Field.field @"symbol"
symbols ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "symbols" a) =>
  Lens.Family2.LensLike' f s a
symbols = Data.ProtoLens.Field.field @"symbols"
syntaxType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "syntaxType" a) =>
  Lens.Family2.LensLike' f s a
syntaxType = Data.ProtoLens.Field.field @"syntaxType"
timestamp ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timestamp" a) =>
  Lens.Family2.LensLike' f s a
timestamp = Data.ProtoLens.Field.field @"timestamp"
to ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "to" a) =>
  Lens.Family2.LensLike' f s a
to = Data.ProtoLens.Field.field @"to"
utf16CodeUnitSpan ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "utf16CodeUnitSpan" a) =>
  Lens.Family2.LensLike' f s a
utf16CodeUnitSpan = Data.ProtoLens.Field.field @"utf16CodeUnitSpan"
vec'blobs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'blobs" a) =>
  Lens.Family2.LensLike' f s a
vec'blobs = Data.ProtoLens.Field.field @"vec'blobs"
vec'endingScopeStack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'endingScopeStack" a) =>
  Lens.Family2.LensLike' f s a
vec'endingScopeStack
  = Data.ProtoLens.Field.field @"vec'endingScopeStack"
vec'endingSymbolStack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'endingSymbolStack" a) =>
  Lens.Family2.LensLike' f s a
vec'endingSymbolStack
  = Data.ProtoLens.Field.field @"vec'endingSymbolStack"
vec'errors ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'errors" a) =>
  Lens.Family2.LensLike' f s a
vec'errors = Data.ProtoLens.Field.field @"vec'errors"
vec'files ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'files" a) =>
  Lens.Family2.LensLike' f s a
vec'files = Data.ProtoLens.Field.field @"vec'files"
vec'nodes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'nodes" a) =>
  Lens.Family2.LensLike' f s a
vec'nodes = Data.ProtoLens.Field.field @"vec'nodes"
vec'paths ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'paths" a) =>
  Lens.Family2.LensLike' f s a
vec'paths = Data.ProtoLens.Field.field @"vec'paths"
vec'startingSymbolStack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'startingSymbolStack" a) =>
  Lens.Family2.LensLike' f s a
vec'startingSymbolStack
  = Data.ProtoLens.Field.field @"vec'startingSymbolStack"
vec'symbols ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'symbols" a) =>
  Lens.Family2.LensLike' f s a
vec'symbols = Data.ProtoLens.Field.field @"vec'symbols"