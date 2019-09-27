{- This file was auto-generated from semantic.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Semantic_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens
       as Data.ProtoLens
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
import qualified Data.ProtoLens.Runtime.Lens.Labels as Lens.Labels
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read

after ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "after" a) =>
        Lens.Family2.LensLike' f s a
after
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "after")
afterSpan ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "afterSpan" a) =>
            Lens.Family2.LensLike' f s a
afterSpan
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "afterSpan")
afterTerm ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "afterTerm" a) =>
            Lens.Family2.LensLike' f s a
afterTerm
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "afterTerm")
before ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "before" a) =>
         Lens.Family2.LensLike' f s a
before
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "before")
beforeSpan ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "beforeSpan" a) =>
             Lens.Family2.LensLike' f s a
beforeSpan
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "beforeSpan")
beforeTerm ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "beforeTerm" a) =>
             Lens.Family2.LensLike' f s a
beforeTerm
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "beforeTerm")
blobOid ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "blobOid" a) =>
          Lens.Family2.LensLike' f s a
blobOid
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "blobOid")
blobs ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "blobs" a) =>
        Lens.Family2.LensLike' f s a
blobs
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "blobs")
category ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "category" a) =>
           Lens.Family2.LensLike' f s a
category
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "category")
changeType ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "changeType" a) =>
             Lens.Family2.LensLike' f s a
changeType
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "changeType")
changes ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "changes" a) =>
          Lens.Family2.LensLike' f s a
changes
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "changes")
column ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "column" a) =>
         Lens.Family2.LensLike' f s a
column
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "column")
content ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "content" a) =>
          Lens.Family2.LensLike' f s a
content
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "content")
deleted ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "deleted" a) =>
          Lens.Family2.LensLike' f s a
deleted
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "deleted")
diffVertexId ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "diffVertexId" a) =>
               Lens.Family2.LensLike' f s a
diffVertexId
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "diffVertexId")
docs ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "docs" a) =>
       Lens.Family2.LensLike' f s a
docs
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docs")
docstring ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "docstring" a) =>
            Lens.Family2.LensLike' f s a
docstring
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docstring")
edges ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "edges" a) =>
        Lens.Family2.LensLike' f s a
edges
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "edges")
end ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "end" a) =>
      Lens.Family2.LensLike' f s a
end
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "end")
error ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "error" a) =>
        Lens.Family2.LensLike' f s a
error
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "error")
errors ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "errors" a) =>
         Lens.Family2.LensLike' f s a
errors
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "errors")
files ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "files" a) =>
        Lens.Family2.LensLike' f s a
files
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "files")
hostname ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "hostname" a) =>
           Lens.Family2.LensLike' f s a
hostname
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "hostname")
inserted ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "inserted" a) =>
           Lens.Family2.LensLike' f s a
inserted
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "inserted")
kind ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "kind" a) =>
       Lens.Family2.LensLike' f s a
kind
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "kind")
language ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "language" a) =>
           Lens.Family2.LensLike' f s a
language
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "language")
line ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "line" a) =>
       Lens.Family2.LensLike' f s a
line
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "line")
maybe'after ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'after" a) =>
              Lens.Family2.LensLike' f s a
maybe'after
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'after")
maybe'afterSpan ::
                forall f s a .
                  (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'afterSpan" a) =>
                  Lens.Family2.LensLike' f s a
maybe'afterSpan
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'afterSpan")
maybe'before ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'before" a) =>
               Lens.Family2.LensLike' f s a
maybe'before
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'before")
maybe'beforeSpan ::
                 forall f s a .
                   (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'beforeSpan" a) =>
                   Lens.Family2.LensLike' f s a
maybe'beforeSpan
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'beforeSpan")
maybe'deleted ::
              forall f s a .
                (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'deleted" a) =>
                Lens.Family2.LensLike' f s a
maybe'deleted
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'deleted")
maybe'diffTerm ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'diffTerm" a) =>
                 Lens.Family2.LensLike' f s a
maybe'diffTerm
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'diffTerm")
maybe'docs ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'docs" a) =>
             Lens.Family2.LensLike' f s a
maybe'docs
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'docs")
maybe'end ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'end" a) =>
            Lens.Family2.LensLike' f s a
maybe'end
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'end")
maybe'inserted ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'inserted" a) =>
                 Lens.Family2.LensLike' f s a
maybe'inserted
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'inserted")
maybe'merged ::
             forall f s a .
               (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'merged" a) =>
               Lens.Family2.LensLike' f s a
maybe'merged
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'merged")
maybe'replaced ::
               forall f s a .
                 (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'replaced" a) =>
                 Lens.Family2.LensLike' f s a
maybe'replaced
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'replaced")
maybe'span ::
           forall f s a .
             (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'span" a) =>
             Lens.Family2.LensLike' f s a
maybe'span
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'span")
maybe'start ::
            forall f s a .
              (Prelude.Functor f, Lens.Labels.HasLens' s "maybe'start" a) =>
              Lens.Family2.LensLike' f s a
maybe'start
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'start")
merged ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "merged" a) =>
         Lens.Family2.LensLike' f s a
merged
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "merged")
path ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "path" a) =>
       Lens.Family2.LensLike' f s a
path
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "path")
replaced ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "replaced" a) =>
           Lens.Family2.LensLike' f s a
replaced
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "replaced")
service ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "service" a) =>
          Lens.Family2.LensLike' f s a
service
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "service")
sha ::
    forall f s a .
      (Prelude.Functor f, Lens.Labels.HasLens' s "sha" a) =>
      Lens.Family2.LensLike' f s a
sha
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "sha")
source ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "source" a) =>
         Lens.Family2.LensLike' f s a
source
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "source")
span ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "span" a) =>
       Lens.Family2.LensLike' f s a
span
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "span")
start ::
      forall f s a .
        (Prelude.Functor f, Lens.Labels.HasLens' s "start" a) =>
        Lens.Family2.LensLike' f s a
start
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "start")
status ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "status" a) =>
         Lens.Family2.LensLike' f s a
status
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "status")
symbol ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "symbol" a) =>
         Lens.Family2.LensLike' f s a
symbol
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "symbol")
symbols ::
        forall f s a .
          (Prelude.Functor f, Lens.Labels.HasLens' s "symbols" a) =>
          Lens.Family2.LensLike' f s a
symbols
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "symbols")
target ::
       forall f s a .
         (Prelude.Functor f, Lens.Labels.HasLens' s "target" a) =>
         Lens.Family2.LensLike' f s a
target
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "target")
term ::
     forall f s a .
       (Prelude.Functor f, Lens.Labels.HasLens' s "term" a) =>
       Lens.Family2.LensLike' f s a
term
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "term")
timestamp ::
          forall f s a .
            (Prelude.Functor f, Lens.Labels.HasLens' s "timestamp" a) =>
            Lens.Family2.LensLike' f s a
timestamp
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "timestamp")
vertexId ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "vertexId" a) =>
           Lens.Family2.LensLike' f s a
vertexId
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "vertexId")
vertices ::
         forall f s a .
           (Prelude.Functor f, Lens.Labels.HasLens' s "vertices" a) =>
           Lens.Family2.LensLike' f s a
vertices
  = Lens.Labels.lensOf'
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "vertices")