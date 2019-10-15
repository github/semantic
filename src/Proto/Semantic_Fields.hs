{- This file was auto-generated from semantic.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds,
  BangPatterns, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-} -- Manually added for semantic's project settings
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Semantic_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes
       as Data.ProtoLens.Encoding.Bytes
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing
       as Data.ProtoLens.Encoding.Growing
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe
       as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire
       as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field
       as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified
       Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types
       as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8
       as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding
       as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic
       as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed
       as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read

after ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "after" a) =>
        Lens.Family2.LensLike' f s a
after = Data.ProtoLens.Field.field @"after"
afterSpan ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "afterSpan" a) =>
            Lens.Family2.LensLike' f s a
afterSpan = Data.ProtoLens.Field.field @"afterSpan"
afterTerm ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "afterTerm" a) =>
            Lens.Family2.LensLike' f s a
afterTerm = Data.ProtoLens.Field.field @"afterTerm"
before ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "before" a) =>
         Lens.Family2.LensLike' f s a
before = Data.ProtoLens.Field.field @"before"
beforeSpan ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "beforeSpan" a) =>
             Lens.Family2.LensLike' f s a
beforeSpan = Data.ProtoLens.Field.field @"beforeSpan"
beforeTerm ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "beforeTerm" a) =>
             Lens.Family2.LensLike' f s a
beforeTerm = Data.ProtoLens.Field.field @"beforeTerm"
blobOid ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "blobOid" a) =>
          Lens.Family2.LensLike' f s a
blobOid = Data.ProtoLens.Field.field @"blobOid"
blobs ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "blobs" a) =>
        Lens.Family2.LensLike' f s a
blobs = Data.ProtoLens.Field.field @"blobs"
category ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "category" a) =>
           Lens.Family2.LensLike' f s a
category = Data.ProtoLens.Field.field @"category"
changeType ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "changeType" a) =>
             Lens.Family2.LensLike' f s a
changeType = Data.ProtoLens.Field.field @"changeType"
changes ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "changes" a) =>
          Lens.Family2.LensLike' f s a
changes = Data.ProtoLens.Field.field @"changes"
column ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "column" a) =>
         Lens.Family2.LensLike' f s a
column = Data.ProtoLens.Field.field @"column"
content ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "content" a) =>
          Lens.Family2.LensLike' f s a
content = Data.ProtoLens.Field.field @"content"
deleted ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "deleted" a) =>
          Lens.Family2.LensLike' f s a
deleted = Data.ProtoLens.Field.field @"deleted"
diffVertexId ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "diffVertexId" a) =>
               Lens.Family2.LensLike' f s a
diffVertexId = Data.ProtoLens.Field.field @"diffVertexId"
docs ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "docs" a) =>
       Lens.Family2.LensLike' f s a
docs = Data.ProtoLens.Field.field @"docs"
docstring ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "docstring" a) =>
            Lens.Family2.LensLike' f s a
docstring = Data.ProtoLens.Field.field @"docstring"
edges ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "edges" a) =>
        Lens.Family2.LensLike' f s a
edges = Data.ProtoLens.Field.field @"edges"
end ::
    forall f s a .
      (Prelude.Functor f, Data.ProtoLens.Field.HasField s "end" a) =>
      Lens.Family2.LensLike' f s a
end = Data.ProtoLens.Field.field @"end"
error ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "error" a) =>
        Lens.Family2.LensLike' f s a
error = Data.ProtoLens.Field.field @"error"
errors ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "errors" a) =>
         Lens.Family2.LensLike' f s a
errors = Data.ProtoLens.Field.field @"errors"
files ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "files" a) =>
        Lens.Family2.LensLike' f s a
files = Data.ProtoLens.Field.field @"files"
hostname ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "hostname" a) =>
           Lens.Family2.LensLike' f s a
hostname = Data.ProtoLens.Field.field @"hostname"
inserted ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "inserted" a) =>
           Lens.Family2.LensLike' f s a
inserted = Data.ProtoLens.Field.field @"inserted"
kind ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "kind" a) =>
       Lens.Family2.LensLike' f s a
kind = Data.ProtoLens.Field.field @"kind"
language ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "language" a) =>
           Lens.Family2.LensLike' f s a
language = Data.ProtoLens.Field.field @"language"
line ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "line" a) =>
       Lens.Family2.LensLike' f s a
line = Data.ProtoLens.Field.field @"line"
maybe'after ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "maybe'after" a) =>
              Lens.Family2.LensLike' f s a
maybe'after = Data.ProtoLens.Field.field @"maybe'after"
maybe'afterSpan ::
                forall f s a .
                  (Prelude.Functor f,
                   Data.ProtoLens.Field.HasField s "maybe'afterSpan" a) =>
                  Lens.Family2.LensLike' f s a
maybe'afterSpan = Data.ProtoLens.Field.field @"maybe'afterSpan"
maybe'before ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "maybe'before" a) =>
               Lens.Family2.LensLike' f s a
maybe'before = Data.ProtoLens.Field.field @"maybe'before"
maybe'beforeSpan ::
                 forall f s a .
                   (Prelude.Functor f,
                    Data.ProtoLens.Field.HasField s "maybe'beforeSpan" a) =>
                   Lens.Family2.LensLike' f s a
maybe'beforeSpan = Data.ProtoLens.Field.field @"maybe'beforeSpan"
maybe'deleted ::
              forall f s a .
                (Prelude.Functor f,
                 Data.ProtoLens.Field.HasField s "maybe'deleted" a) =>
                Lens.Family2.LensLike' f s a
maybe'deleted = Data.ProtoLens.Field.field @"maybe'deleted"
maybe'diffTerm ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "maybe'diffTerm" a) =>
                 Lens.Family2.LensLike' f s a
maybe'diffTerm = Data.ProtoLens.Field.field @"maybe'diffTerm"
maybe'docs ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "maybe'docs" a) =>
             Lens.Family2.LensLike' f s a
maybe'docs = Data.ProtoLens.Field.field @"maybe'docs"
maybe'end ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "maybe'end" a) =>
            Lens.Family2.LensLike' f s a
maybe'end = Data.ProtoLens.Field.field @"maybe'end"
maybe'inserted ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "maybe'inserted" a) =>
                 Lens.Family2.LensLike' f s a
maybe'inserted = Data.ProtoLens.Field.field @"maybe'inserted"
maybe'merged ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "maybe'merged" a) =>
               Lens.Family2.LensLike' f s a
maybe'merged = Data.ProtoLens.Field.field @"maybe'merged"
maybe'replaced ::
               forall f s a .
                 (Prelude.Functor f,
                  Data.ProtoLens.Field.HasField s "maybe'replaced" a) =>
                 Lens.Family2.LensLike' f s a
maybe'replaced = Data.ProtoLens.Field.field @"maybe'replaced"
maybe'span ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "maybe'span" a) =>
             Lens.Family2.LensLike' f s a
maybe'span = Data.ProtoLens.Field.field @"maybe'span"
maybe'start ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "maybe'start" a) =>
              Lens.Family2.LensLike' f s a
maybe'start = Data.ProtoLens.Field.field @"maybe'start"
merged ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "merged" a) =>
         Lens.Family2.LensLike' f s a
merged = Data.ProtoLens.Field.field @"merged"
path ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "path" a) =>
       Lens.Family2.LensLike' f s a
path = Data.ProtoLens.Field.field @"path"
replaced ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "replaced" a) =>
           Lens.Family2.LensLike' f s a
replaced = Data.ProtoLens.Field.field @"replaced"
service ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "service" a) =>
          Lens.Family2.LensLike' f s a
service = Data.ProtoLens.Field.field @"service"
sha ::
    forall f s a .
      (Prelude.Functor f, Data.ProtoLens.Field.HasField s "sha" a) =>
      Lens.Family2.LensLike' f s a
sha = Data.ProtoLens.Field.field @"sha"
source ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "source" a) =>
         Lens.Family2.LensLike' f s a
source = Data.ProtoLens.Field.field @"source"
span ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "span" a) =>
       Lens.Family2.LensLike' f s a
span = Data.ProtoLens.Field.field @"span"
start ::
      forall f s a .
        (Prelude.Functor f, Data.ProtoLens.Field.HasField s "start" a) =>
        Lens.Family2.LensLike' f s a
start = Data.ProtoLens.Field.field @"start"
status ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "status" a) =>
         Lens.Family2.LensLike' f s a
status = Data.ProtoLens.Field.field @"status"
symbol ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "symbol" a) =>
         Lens.Family2.LensLike' f s a
symbol = Data.ProtoLens.Field.field @"symbol"
symbols ::
        forall f s a .
          (Prelude.Functor f, Data.ProtoLens.Field.HasField s "symbols" a) =>
          Lens.Family2.LensLike' f s a
symbols = Data.ProtoLens.Field.field @"symbols"
target ::
       forall f s a .
         (Prelude.Functor f, Data.ProtoLens.Field.HasField s "target" a) =>
         Lens.Family2.LensLike' f s a
target = Data.ProtoLens.Field.field @"target"
term ::
     forall f s a .
       (Prelude.Functor f, Data.ProtoLens.Field.HasField s "term" a) =>
       Lens.Family2.LensLike' f s a
term = Data.ProtoLens.Field.field @"term"
timestamp ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "timestamp" a) =>
            Lens.Family2.LensLike' f s a
timestamp = Data.ProtoLens.Field.field @"timestamp"
vec'blobs ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "vec'blobs" a) =>
            Lens.Family2.LensLike' f s a
vec'blobs = Data.ProtoLens.Field.field @"vec'blobs"
vec'changes ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "vec'changes" a) =>
              Lens.Family2.LensLike' f s a
vec'changes = Data.ProtoLens.Field.field @"vec'changes"
vec'edges ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "vec'edges" a) =>
            Lens.Family2.LensLike' f s a
vec'edges = Data.ProtoLens.Field.field @"vec'edges"
vec'errors ::
           forall f s a .
             (Prelude.Functor f,
              Data.ProtoLens.Field.HasField s "vec'errors" a) =>
             Lens.Family2.LensLike' f s a
vec'errors = Data.ProtoLens.Field.field @"vec'errors"
vec'files ::
          forall f s a .
            (Prelude.Functor f,
             Data.ProtoLens.Field.HasField s "vec'files" a) =>
            Lens.Family2.LensLike' f s a
vec'files = Data.ProtoLens.Field.field @"vec'files"
vec'symbols ::
            forall f s a .
              (Prelude.Functor f,
               Data.ProtoLens.Field.HasField s "vec'symbols" a) =>
              Lens.Family2.LensLike' f s a
vec'symbols = Data.ProtoLens.Field.field @"vec'symbols"
vec'vertices ::
             forall f s a .
               (Prelude.Functor f,
                Data.ProtoLens.Field.HasField s "vec'vertices" a) =>
               Lens.Family2.LensLike' f s a
vec'vertices = Data.ProtoLens.Field.field @"vec'vertices"
vertexId ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "vertexId" a) =>
           Lens.Family2.LensLike' f s a
vertexId = Data.ProtoLens.Field.field @"vertexId"
vertices ::
         forall f s a .
           (Prelude.Functor f,
            Data.ProtoLens.Field.HasField s "vertices" a) =>
           Lens.Family2.LensLike' f s a
vertices = Data.ProtoLens.Field.field @"vertices"
