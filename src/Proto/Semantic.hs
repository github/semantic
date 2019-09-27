{- This file was auto-generated from semantic.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, GeneralizedNewtypeDeriving,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports#-}
module Proto.Semantic
       (Blob(), BlobPair(), ChangeType(..), ChangeType(),
        ChangeType'UnrecognizedValue, DeletedTerm(), DiffTreeEdge(),
        DiffTreeFileGraph(), DiffTreeGraphResponse(), DiffTreeRequest(),
        DiffTreeTOCResponse(), DiffTreeVertex(),
        DiffTreeVertex'DiffTerm(..), _DiffTreeVertex'Deleted,
        _DiffTreeVertex'Inserted, _DiffTreeVertex'Replaced,
        _DiffTreeVertex'Merged, Docstring(), File(), InsertedTerm(),
        MergedTerm(), ParseError(), ParseTreeFileGraph(),
        ParseTreeGraphResponse(), ParseTreeRequest(),
        ParseTreeSymbolResponse(), PingRequest(), PingResponse(),
        Position(), ReplacedTerm(), Span(), Symbol(), TOCSummaryChange(),
        TOCSummaryError(), TOCSummaryFile(), TermEdge(), TermVertex())
       where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq
       as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Lens.Labels.Prism
       as Lens.Labels.Prism
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

{- | Fields :

    * 'Proto.Semantic_Fields.content' @:: Lens' Blob Data.Text.Text@
    * 'Proto.Semantic_Fields.path' @:: Lens' Blob Data.Text.Text@
    * 'Proto.Semantic_Fields.language' @:: Lens' Blob Data.Text.Text@
 -}
data Blob = Blob{_Blob'content :: !Data.Text.Text,
                 _Blob'path :: !Data.Text.Text, _Blob'language :: !Data.Text.Text,
                 _Blob'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Blob where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Blob "content" (Data.Text.Text) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Blob'content
                 (\ x__ y__ -> x__{_Blob'content = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Blob "path" (Data.Text.Text) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Blob'path
                 (\ x__ y__ -> x__{_Blob'path = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Blob "language" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Blob'language
                 (\ x__ y__ -> x__{_Blob'language = y__}))
              Prelude.id
instance Data.ProtoLens.Message Blob where
        messageName _ = Data.Text.pack "github.semantic.Blob"
        fieldsByTag
          = let content__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "content"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "content")))
                      :: Data.ProtoLens.FieldDescriptor Blob
                path__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "path"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "path")))
                      :: Data.ProtoLens.FieldDescriptor Blob
                language__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "language"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "language")))
                      :: Data.ProtoLens.FieldDescriptor Blob
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, content__field_descriptor),
                 (Data.ProtoLens.Tag 2, path__field_descriptor),
                 (Data.ProtoLens.Tag 3, language__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Blob'_unknownFields
              (\ x__ y__ -> x__{_Blob'_unknownFields = y__})
        defMessage
          = Blob{_Blob'content = Data.ProtoLens.fieldDefault,
                 _Blob'path = Data.ProtoLens.fieldDefault,
                 _Blob'language = Data.ProtoLens.fieldDefault,
                 _Blob'_unknownFields = ([])}
instance Control.DeepSeq.NFData Blob where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Blob'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Blob'content x__)
                   (Control.DeepSeq.deepseq (_Blob'path x__)
                      (Control.DeepSeq.deepseq (_Blob'language x__) (()))))
{- | Fields :

    * 'Proto.Semantic_Fields.before' @:: Lens' BlobPair Blob@
    * 'Proto.Semantic_Fields.maybe'before' @:: Lens' BlobPair (Prelude.Maybe Blob)@
    * 'Proto.Semantic_Fields.after' @:: Lens' BlobPair Blob@
    * 'Proto.Semantic_Fields.maybe'after' @:: Lens' BlobPair (Prelude.Maybe Blob)@
 -}
data BlobPair = BlobPair{_BlobPair'before :: !(Prelude.Maybe Blob),
                         _BlobPair'after :: !(Prelude.Maybe Blob),
                         _BlobPair'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BlobPair where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' BlobPair "before" (Blob) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BlobPair'before
                 (\ x__ y__ -> x__{_BlobPair'before = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' BlobPair "maybe'before"
           (Prelude.Maybe Blob)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BlobPair'before
                 (\ x__ y__ -> x__{_BlobPair'before = y__}))
              Prelude.id
instance Lens.Labels.HasLens' BlobPair "after" (Blob) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BlobPair'after
                 (\ x__ y__ -> x__{_BlobPair'after = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' BlobPair "maybe'after"
           (Prelude.Maybe Blob)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _BlobPair'after
                 (\ x__ y__ -> x__{_BlobPair'after = y__}))
              Prelude.id
instance Data.ProtoLens.Message BlobPair where
        messageName _ = Data.Text.pack "github.semantic.BlobPair"
        fieldsByTag
          = let before__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "before"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Blob)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'before")))
                      :: Data.ProtoLens.FieldDescriptor BlobPair
                after__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "after"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Blob)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'after")))
                      :: Data.ProtoLens.FieldDescriptor BlobPair
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, before__field_descriptor),
                 (Data.ProtoLens.Tag 2, after__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _BlobPair'_unknownFields
              (\ x__ y__ -> x__{_BlobPair'_unknownFields = y__})
        defMessage
          = BlobPair{_BlobPair'before = Prelude.Nothing,
                     _BlobPair'after = Prelude.Nothing, _BlobPair'_unknownFields = ([])}
instance Control.DeepSeq.NFData BlobPair where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_BlobPair'_unknownFields x__)
                (Control.DeepSeq.deepseq (_BlobPair'before x__)
                   (Control.DeepSeq.deepseq (_BlobPair'after x__) (())))
newtype ChangeType'UnrecognizedValue = ChangeType'UnrecognizedValue Data.Int.Int32
                                         deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
data ChangeType = NONE
                | ADDED
                | REMOVED
                | MODIFIED
                | ChangeType'Unrecognized !ChangeType'UnrecognizedValue
                    deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum ChangeType where
        maybeToEnum 0 = Prelude.Just NONE
        maybeToEnum 1 = Prelude.Just ADDED
        maybeToEnum 2 = Prelude.Just REMOVED
        maybeToEnum 3 = Prelude.Just MODIFIED
        maybeToEnum k
          = Prelude.Just
              (ChangeType'Unrecognized
                 (ChangeType'UnrecognizedValue (Prelude.fromIntegral k)))
        showEnum NONE = "NONE"
        showEnum ADDED = "ADDED"
        showEnum REMOVED = "REMOVED"
        showEnum MODIFIED = "MODIFIED"
        showEnum (ChangeType'Unrecognized (ChangeType'UnrecognizedValue k))
          = Prelude.show k
        readEnum k
          | (Prelude.==) k "NONE" = Prelude.Just NONE
          | (Prelude.==) k "ADDED" = Prelude.Just ADDED
          | (Prelude.==) k "REMOVED" = Prelude.Just REMOVED
          | (Prelude.==) k "MODIFIED" = Prelude.Just MODIFIED
        readEnum k
          = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded ChangeType where
        minBound = NONE
        maxBound = MODIFIED
instance Prelude.Enum ChangeType where
        toEnum k__
          = Prelude.maybe
              (Prelude.error
                 ((Prelude.++) "toEnum: unknown value for enum ChangeType: "
                    (Prelude.show k__)))
              Prelude.id
              (Data.ProtoLens.maybeToEnum k__)
        fromEnum NONE = 0
        fromEnum ADDED = 1
        fromEnum REMOVED = 2
        fromEnum MODIFIED = 3
        fromEnum (ChangeType'Unrecognized (ChangeType'UnrecognizedValue k))
          = Prelude.fromIntegral k
        succ MODIFIED
          = Prelude.error
              "ChangeType.succ: bad argument MODIFIED. This value would be out of bounds."
        succ NONE = ADDED
        succ ADDED = REMOVED
        succ REMOVED = MODIFIED
        succ (ChangeType'Unrecognized _)
          = Prelude.error "ChangeType.succ: bad argument: unrecognized value"
        pred NONE
          = Prelude.error
              "ChangeType.pred: bad argument NONE. This value would be out of bounds."
        pred ADDED = NONE
        pred REMOVED = ADDED
        pred MODIFIED = REMOVED
        pred (ChangeType'Unrecognized _)
          = Prelude.error "ChangeType.pred: bad argument: unrecognized value"
        enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
        enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
        enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
        enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault ChangeType where
        fieldDefault = NONE
instance Control.DeepSeq.NFData ChangeType where
        rnf x__ = Prelude.seq x__ (())
{- | Fields :

    * 'Proto.Semantic_Fields.term' @:: Lens' DeletedTerm Data.Text.Text@
    * 'Proto.Semantic_Fields.span' @:: Lens' DeletedTerm Span@
    * 'Proto.Semantic_Fields.maybe'span' @:: Lens' DeletedTerm (Prelude.Maybe Span)@
 -}
data DeletedTerm = DeletedTerm{_DeletedTerm'term ::
                               !Data.Text.Text,
                               _DeletedTerm'span :: !(Prelude.Maybe Span),
                               _DeletedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeletedTerm where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' DeletedTerm "term" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeletedTerm'term
                 (\ x__ y__ -> x__{_DeletedTerm'term = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DeletedTerm "span" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeletedTerm'span
                 (\ x__ y__ -> x__{_DeletedTerm'span = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' DeletedTerm "maybe'span"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DeletedTerm'span
                 (\ x__ y__ -> x__{_DeletedTerm'span = y__}))
              Prelude.id
instance Data.ProtoLens.Message DeletedTerm where
        messageName _ = Data.Text.pack "github.semantic.DeletedTerm"
        fieldsByTag
          = let term__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "term"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "term")))
                      :: Data.ProtoLens.FieldDescriptor DeletedTerm
                span__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'span")))
                      :: Data.ProtoLens.FieldDescriptor DeletedTerm
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, term__field_descriptor),
                 (Data.ProtoLens.Tag 2, span__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DeletedTerm'_unknownFields
              (\ x__ y__ -> x__{_DeletedTerm'_unknownFields = y__})
        defMessage
          = DeletedTerm{_DeletedTerm'term = Data.ProtoLens.fieldDefault,
                        _DeletedTerm'span = Prelude.Nothing,
                        _DeletedTerm'_unknownFields = ([])}
instance Control.DeepSeq.NFData DeletedTerm where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DeletedTerm'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DeletedTerm'term x__)
                   (Control.DeepSeq.deepseq (_DeletedTerm'span x__) (())))
{- | Fields :

    * 'Proto.Semantic_Fields.source' @:: Lens' DiffTreeEdge Data.Int.Int32@
    * 'Proto.Semantic_Fields.target' @:: Lens' DiffTreeEdge Data.Int.Int32@
 -}
data DiffTreeEdge = DiffTreeEdge{_DiffTreeEdge'source ::
                                 !Data.Int.Int32,
                                 _DiffTreeEdge'target :: !Data.Int.Int32,
                                 _DiffTreeEdge'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeEdge where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' DiffTreeEdge "source"
           (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeEdge'source
                 (\ x__ y__ -> x__{_DiffTreeEdge'source = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DiffTreeEdge "target"
           (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeEdge'target
                 (\ x__ y__ -> x__{_DiffTreeEdge'target = y__}))
              Prelude.id
instance Data.ProtoLens.Message DiffTreeEdge where
        messageName _ = Data.Text.pack "github.semantic.DiffTreeEdge"
        fieldsByTag
          = let source__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "source"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "source")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeEdge
                target__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "target"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "target")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeEdge
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, source__field_descriptor),
                 (Data.ProtoLens.Tag 2, target__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DiffTreeEdge'_unknownFields
              (\ x__ y__ -> x__{_DiffTreeEdge'_unknownFields = y__})
        defMessage
          = DiffTreeEdge{_DiffTreeEdge'source = Data.ProtoLens.fieldDefault,
                         _DiffTreeEdge'target = Data.ProtoLens.fieldDefault,
                         _DiffTreeEdge'_unknownFields = ([])}
instance Control.DeepSeq.NFData DiffTreeEdge where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DiffTreeEdge'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DiffTreeEdge'source x__)
                   (Control.DeepSeq.deepseq (_DiffTreeEdge'target x__) (())))
{- | Fields :

    * 'Proto.Semantic_Fields.path' @:: Lens' DiffTreeFileGraph Data.Text.Text@
    * 'Proto.Semantic_Fields.language' @:: Lens' DiffTreeFileGraph Data.Text.Text@
    * 'Proto.Semantic_Fields.vertices' @:: Lens' DiffTreeFileGraph [DiffTreeVertex]@
    * 'Proto.Semantic_Fields.edges' @:: Lens' DiffTreeFileGraph [DiffTreeEdge]@
    * 'Proto.Semantic_Fields.errors' @:: Lens' DiffTreeFileGraph [ParseError]@
 -}
data DiffTreeFileGraph = DiffTreeFileGraph{_DiffTreeFileGraph'path
                                           :: !Data.Text.Text,
                                           _DiffTreeFileGraph'language :: !Data.Text.Text,
                                           _DiffTreeFileGraph'vertices :: ![DiffTreeVertex],
                                           _DiffTreeFileGraph'edges :: ![DiffTreeEdge],
                                           _DiffTreeFileGraph'errors :: ![ParseError],
                                           _DiffTreeFileGraph'_unknownFields ::
                                           !Data.ProtoLens.FieldSet}
                           deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeFileGraph where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' DiffTreeFileGraph "path"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeFileGraph'path
                 (\ x__ y__ -> x__{_DiffTreeFileGraph'path = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DiffTreeFileGraph "language"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeFileGraph'language
                 (\ x__ y__ -> x__{_DiffTreeFileGraph'language = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DiffTreeFileGraph "vertices"
           ([DiffTreeVertex])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeFileGraph'vertices
                 (\ x__ y__ -> x__{_DiffTreeFileGraph'vertices = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DiffTreeFileGraph "edges"
           ([DiffTreeEdge])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeFileGraph'edges
                 (\ x__ y__ -> x__{_DiffTreeFileGraph'edges = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DiffTreeFileGraph "errors"
           ([ParseError])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeFileGraph'errors
                 (\ x__ y__ -> x__{_DiffTreeFileGraph'errors = y__}))
              Prelude.id
instance Data.ProtoLens.Message DiffTreeFileGraph where
        messageName _ = Data.Text.pack "github.semantic.DiffTreeFileGraph"
        fieldsByTag
          = let path__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "path"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "path")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
                language__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "language"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "language")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
                vertices__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vertices"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor DiffTreeVertex)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "vertices")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
                edges__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "edges"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor DiffTreeEdge)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "edges")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
                errors__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "errors"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ParseError)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "errors")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, path__field_descriptor),
                 (Data.ProtoLens.Tag 2, language__field_descriptor),
                 (Data.ProtoLens.Tag 3, vertices__field_descriptor),
                 (Data.ProtoLens.Tag 4, edges__field_descriptor),
                 (Data.ProtoLens.Tag 5, errors__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DiffTreeFileGraph'_unknownFields
              (\ x__ y__ -> x__{_DiffTreeFileGraph'_unknownFields = y__})
        defMessage
          = DiffTreeFileGraph{_DiffTreeFileGraph'path =
                                Data.ProtoLens.fieldDefault,
                              _DiffTreeFileGraph'language = Data.ProtoLens.fieldDefault,
                              _DiffTreeFileGraph'vertices = [], _DiffTreeFileGraph'edges = [],
                              _DiffTreeFileGraph'errors = [],
                              _DiffTreeFileGraph'_unknownFields = ([])}
instance Control.DeepSeq.NFData DiffTreeFileGraph where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DiffTreeFileGraph'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DiffTreeFileGraph'path x__)
                   (Control.DeepSeq.deepseq (_DiffTreeFileGraph'language x__)
                      (Control.DeepSeq.deepseq (_DiffTreeFileGraph'vertices x__)
                         (Control.DeepSeq.deepseq (_DiffTreeFileGraph'edges x__)
                            (Control.DeepSeq.deepseq (_DiffTreeFileGraph'errors x__) (()))))))
{- | Fields :

    * 'Proto.Semantic_Fields.files' @:: Lens' DiffTreeGraphResponse [DiffTreeFileGraph]@
 -}
data DiffTreeGraphResponse = DiffTreeGraphResponse{_DiffTreeGraphResponse'files
                                                   :: ![DiffTreeFileGraph],
                                                   _DiffTreeGraphResponse'_unknownFields ::
                                                   !Data.ProtoLens.FieldSet}
                               deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeGraphResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' DiffTreeGraphResponse "files"
           ([DiffTreeFileGraph])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeGraphResponse'files
                 (\ x__ y__ -> x__{_DiffTreeGraphResponse'files = y__}))
              Prelude.id
instance Data.ProtoLens.Message DiffTreeGraphResponse where
        messageName _
          = Data.Text.pack "github.semantic.DiffTreeGraphResponse"
        fieldsByTag
          = let files__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "files"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor DiffTreeFileGraph)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "files")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeGraphResponse
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DiffTreeGraphResponse'_unknownFields
              (\ x__ y__ -> x__{_DiffTreeGraphResponse'_unknownFields = y__})
        defMessage
          = DiffTreeGraphResponse{_DiffTreeGraphResponse'files = [],
                                  _DiffTreeGraphResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData DiffTreeGraphResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DiffTreeGraphResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DiffTreeGraphResponse'files x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.blobs' @:: Lens' DiffTreeRequest [BlobPair]@
 -}
data DiffTreeRequest = DiffTreeRequest{_DiffTreeRequest'blobs ::
                                       ![BlobPair],
                                       _DiffTreeRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                         deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' DiffTreeRequest "blobs" ([BlobPair])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeRequest'blobs
                 (\ x__ y__ -> x__{_DiffTreeRequest'blobs = y__}))
              Prelude.id
instance Data.ProtoLens.Message DiffTreeRequest where
        messageName _ = Data.Text.pack "github.semantic.DiffTreeRequest"
        fieldsByTag
          = let blobs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "blobs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor BlobPair)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "blobs")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeRequest
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, blobs__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DiffTreeRequest'_unknownFields
              (\ x__ y__ -> x__{_DiffTreeRequest'_unknownFields = y__})
        defMessage
          = DiffTreeRequest{_DiffTreeRequest'blobs = [],
                            _DiffTreeRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData DiffTreeRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DiffTreeRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DiffTreeRequest'blobs x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.files' @:: Lens' DiffTreeTOCResponse [TOCSummaryFile]@
 -}
data DiffTreeTOCResponse = DiffTreeTOCResponse{_DiffTreeTOCResponse'files
                                               :: ![TOCSummaryFile],
                                               _DiffTreeTOCResponse'_unknownFields ::
                                               !Data.ProtoLens.FieldSet}
                             deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeTOCResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' DiffTreeTOCResponse "files"
           ([TOCSummaryFile])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeTOCResponse'files
                 (\ x__ y__ -> x__{_DiffTreeTOCResponse'files = y__}))
              Prelude.id
instance Data.ProtoLens.Message DiffTreeTOCResponse where
        messageName _
          = Data.Text.pack "github.semantic.DiffTreeTOCResponse"
        fieldsByTag
          = let files__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "files"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TOCSummaryFile)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "files")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeTOCResponse
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DiffTreeTOCResponse'_unknownFields
              (\ x__ y__ -> x__{_DiffTreeTOCResponse'_unknownFields = y__})
        defMessage
          = DiffTreeTOCResponse{_DiffTreeTOCResponse'files = [],
                                _DiffTreeTOCResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData DiffTreeTOCResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DiffTreeTOCResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DiffTreeTOCResponse'files x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.diffVertexId' @:: Lens' DiffTreeVertex Data.Int.Int32@
    * 'Proto.Semantic_Fields.maybe'diffTerm' @:: Lens' DiffTreeVertex (Prelude.Maybe DiffTreeVertex'DiffTerm)@
    * 'Proto.Semantic_Fields.maybe'deleted' @:: Lens' DiffTreeVertex (Prelude.Maybe DeletedTerm)@
    * 'Proto.Semantic_Fields.deleted' @:: Lens' DiffTreeVertex DeletedTerm@
    * 'Proto.Semantic_Fields.maybe'inserted' @:: Lens' DiffTreeVertex (Prelude.Maybe InsertedTerm)@
    * 'Proto.Semantic_Fields.inserted' @:: Lens' DiffTreeVertex InsertedTerm@
    * 'Proto.Semantic_Fields.maybe'replaced' @:: Lens' DiffTreeVertex (Prelude.Maybe ReplacedTerm)@
    * 'Proto.Semantic_Fields.replaced' @:: Lens' DiffTreeVertex ReplacedTerm@
    * 'Proto.Semantic_Fields.maybe'merged' @:: Lens' DiffTreeVertex (Prelude.Maybe MergedTerm)@
    * 'Proto.Semantic_Fields.merged' @:: Lens' DiffTreeVertex MergedTerm@
 -}
data DiffTreeVertex = DiffTreeVertex{_DiffTreeVertex'diffVertexId
                                     :: !Data.Int.Int32,
                                     _DiffTreeVertex'diffTerm ::
                                     !(Prelude.Maybe DiffTreeVertex'DiffTerm),
                                     _DiffTreeVertex'_unknownFields :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeVertex where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
data DiffTreeVertex'DiffTerm = DiffTreeVertex'Deleted !DeletedTerm
                             | DiffTreeVertex'Inserted !InsertedTerm
                             | DiffTreeVertex'Replaced !ReplacedTerm
                             | DiffTreeVertex'Merged !MergedTerm
                                 deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Lens.Labels.HasLens' DiffTreeVertex "diffVertexId"
           (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffVertexId
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffVertexId = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DiffTreeVertex "maybe'diffTerm"
           (Prelude.Maybe DiffTreeVertex'DiffTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              Prelude.id
instance Lens.Labels.HasLens' DiffTreeVertex "maybe'deleted"
           (Prelude.Maybe DeletedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (DiffTreeVertex'Deleted x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap DiffTreeVertex'Deleted y__))
instance Lens.Labels.HasLens' DiffTreeVertex "deleted"
           (DeletedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              ((Prelude..)
                 (Lens.Family2.Unchecked.lens
                    (\ x__ ->
                       case x__ of
                           Prelude.Just (DiffTreeVertex'Deleted x__val) -> Prelude.Just x__val
                           _otherwise -> Prelude.Nothing)
                    (\ _ y__ -> Prelude.fmap DiffTreeVertex'Deleted y__))
                 (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Lens.Labels.HasLens' DiffTreeVertex "maybe'inserted"
           (Prelude.Maybe InsertedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (DiffTreeVertex'Inserted x__val) -> Prelude.Just
                                                                           x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap DiffTreeVertex'Inserted y__))
instance Lens.Labels.HasLens' DiffTreeVertex "inserted"
           (InsertedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              ((Prelude..)
                 (Lens.Family2.Unchecked.lens
                    (\ x__ ->
                       case x__ of
                           Prelude.Just (DiffTreeVertex'Inserted x__val) -> Prelude.Just
                                                                              x__val
                           _otherwise -> Prelude.Nothing)
                    (\ _ y__ -> Prelude.fmap DiffTreeVertex'Inserted y__))
                 (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Lens.Labels.HasLens' DiffTreeVertex "maybe'replaced"
           (Prelude.Maybe ReplacedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (DiffTreeVertex'Replaced x__val) -> Prelude.Just
                                                                           x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap DiffTreeVertex'Replaced y__))
instance Lens.Labels.HasLens' DiffTreeVertex "replaced"
           (ReplacedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              ((Prelude..)
                 (Lens.Family2.Unchecked.lens
                    (\ x__ ->
                       case x__ of
                           Prelude.Just (DiffTreeVertex'Replaced x__val) -> Prelude.Just
                                                                              x__val
                           _otherwise -> Prelude.Nothing)
                    (\ _ y__ -> Prelude.fmap DiffTreeVertex'Replaced y__))
                 (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Lens.Labels.HasLens' DiffTreeVertex "maybe'merged"
           (Prelude.Maybe MergedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              (Lens.Family2.Unchecked.lens
                 (\ x__ ->
                    case x__ of
                        Prelude.Just (DiffTreeVertex'Merged x__val) -> Prelude.Just x__val
                        _otherwise -> Prelude.Nothing)
                 (\ _ y__ -> Prelude.fmap DiffTreeVertex'Merged y__))
instance Lens.Labels.HasLens' DiffTreeVertex "merged" (MergedTerm)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _DiffTreeVertex'diffTerm
                 (\ x__ y__ -> x__{_DiffTreeVertex'diffTerm = y__}))
              ((Prelude..)
                 (Lens.Family2.Unchecked.lens
                    (\ x__ ->
                       case x__ of
                           Prelude.Just (DiffTreeVertex'Merged x__val) -> Prelude.Just x__val
                           _otherwise -> Prelude.Nothing)
                    (\ _ y__ -> Prelude.fmap DiffTreeVertex'Merged y__))
                 (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message DiffTreeVertex where
        messageName _ = Data.Text.pack "github.semantic.DiffTreeVertex"
        fieldsByTag
          = let diffVertexId__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "diff_vertex_id"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "diffVertexId")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeVertex
                deleted__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "deleted"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor DeletedTerm)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'deleted")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeVertex
                inserted__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "inserted"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor InsertedTerm)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'inserted")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeVertex
                replaced__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "replaced"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ReplacedTerm)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'replaced")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeVertex
                merged__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "merged"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor MergedTerm)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'merged")))
                      :: Data.ProtoLens.FieldDescriptor DiffTreeVertex
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, diffVertexId__field_descriptor),
                 (Data.ProtoLens.Tag 2, deleted__field_descriptor),
                 (Data.ProtoLens.Tag 3, inserted__field_descriptor),
                 (Data.ProtoLens.Tag 4, replaced__field_descriptor),
                 (Data.ProtoLens.Tag 5, merged__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _DiffTreeVertex'_unknownFields
              (\ x__ y__ -> x__{_DiffTreeVertex'_unknownFields = y__})
        defMessage
          = DiffTreeVertex{_DiffTreeVertex'diffVertexId =
                             Data.ProtoLens.fieldDefault,
                           _DiffTreeVertex'diffTerm = Prelude.Nothing,
                           _DiffTreeVertex'_unknownFields = ([])}
instance Control.DeepSeq.NFData DiffTreeVertex where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_DiffTreeVertex'_unknownFields x__)
                (Control.DeepSeq.deepseq (_DiffTreeVertex'diffVertexId x__)
                   (Control.DeepSeq.deepseq (_DiffTreeVertex'diffTerm x__) (())))
instance Control.DeepSeq.NFData DiffTreeVertex'DiffTerm where
        rnf (DiffTreeVertex'Deleted x__) = Control.DeepSeq.rnf x__
        rnf (DiffTreeVertex'Inserted x__) = Control.DeepSeq.rnf x__
        rnf (DiffTreeVertex'Replaced x__) = Control.DeepSeq.rnf x__
        rnf (DiffTreeVertex'Merged x__) = Control.DeepSeq.rnf x__
_DiffTreeVertex'Deleted ::
                        Lens.Labels.Prism.Prism' DiffTreeVertex'DiffTerm DeletedTerm
_DiffTreeVertex'Deleted
  = Lens.Labels.Prism.prism' DiffTreeVertex'Deleted
      (\ p__ ->
         case p__ of
             DiffTreeVertex'Deleted p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_DiffTreeVertex'Inserted ::
                         Lens.Labels.Prism.Prism' DiffTreeVertex'DiffTerm InsertedTerm
_DiffTreeVertex'Inserted
  = Lens.Labels.Prism.prism' DiffTreeVertex'Inserted
      (\ p__ ->
         case p__ of
             DiffTreeVertex'Inserted p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_DiffTreeVertex'Replaced ::
                         Lens.Labels.Prism.Prism' DiffTreeVertex'DiffTerm ReplacedTerm
_DiffTreeVertex'Replaced
  = Lens.Labels.Prism.prism' DiffTreeVertex'Replaced
      (\ p__ ->
         case p__ of
             DiffTreeVertex'Replaced p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
_DiffTreeVertex'Merged ::
                       Lens.Labels.Prism.Prism' DiffTreeVertex'DiffTerm MergedTerm
_DiffTreeVertex'Merged
  = Lens.Labels.Prism.prism' DiffTreeVertex'Merged
      (\ p__ ->
         case p__ of
             DiffTreeVertex'Merged p__val -> Prelude.Just p__val
             _otherwise -> Prelude.Nothing)
{- | Fields :

    * 'Proto.Semantic_Fields.docstring' @:: Lens' Docstring Data.Text.Text@
 -}
data Docstring = Docstring{_Docstring'docstring :: !Data.Text.Text,
                           _Docstring'_unknownFields :: !Data.ProtoLens.FieldSet}
                   deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Docstring where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Docstring "docstring"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Docstring'docstring
                 (\ x__ y__ -> x__{_Docstring'docstring = y__}))
              Prelude.id
instance Data.ProtoLens.Message Docstring where
        messageName _ = Data.Text.pack "github.semantic.Docstring"
        fieldsByTag
          = let docstring__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "docstring"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "docstring")))
                      :: Data.ProtoLens.FieldDescriptor Docstring
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, docstring__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Docstring'_unknownFields
              (\ x__ y__ -> x__{_Docstring'_unknownFields = y__})
        defMessage
          = Docstring{_Docstring'docstring = Data.ProtoLens.fieldDefault,
                      _Docstring'_unknownFields = ([])}
instance Control.DeepSeq.NFData Docstring where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Docstring'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Docstring'docstring x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.path' @:: Lens' File Data.Text.Text@
    * 'Proto.Semantic_Fields.language' @:: Lens' File Data.Text.Text@
    * 'Proto.Semantic_Fields.symbols' @:: Lens' File [Symbol]@
    * 'Proto.Semantic_Fields.errors' @:: Lens' File [ParseError]@
    * 'Proto.Semantic_Fields.blobOid' @:: Lens' File Data.Text.Text@
 -}
data File = File{_File'path :: !Data.Text.Text,
                 _File'language :: !Data.Text.Text, _File'symbols :: ![Symbol],
                 _File'errors :: ![ParseError], _File'blobOid :: !Data.Text.Text,
                 _File'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show File where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' File "path" (Data.Text.Text) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _File'path
                 (\ x__ y__ -> x__{_File'path = y__}))
              Prelude.id
instance Lens.Labels.HasLens' File "language" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _File'language
                 (\ x__ y__ -> x__{_File'language = y__}))
              Prelude.id
instance Lens.Labels.HasLens' File "symbols" ([Symbol]) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _File'symbols
                 (\ x__ y__ -> x__{_File'symbols = y__}))
              Prelude.id
instance Lens.Labels.HasLens' File "errors" ([ParseError]) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _File'errors
                 (\ x__ y__ -> x__{_File'errors = y__}))
              Prelude.id
instance Lens.Labels.HasLens' File "blobOid" (Data.Text.Text) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _File'blobOid
                 (\ x__ y__ -> x__{_File'blobOid = y__}))
              Prelude.id
instance Data.ProtoLens.Message File where
        messageName _ = Data.Text.pack "github.semantic.File"
        fieldsByTag
          = let path__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "path"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "path")))
                      :: Data.ProtoLens.FieldDescriptor File
                language__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "language"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "language")))
                      :: Data.ProtoLens.FieldDescriptor File
                symbols__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "symbols"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Symbol)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "symbols")))
                      :: Data.ProtoLens.FieldDescriptor File
                errors__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "errors"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ParseError)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "errors")))
                      :: Data.ProtoLens.FieldDescriptor File
                blobOid__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "blob_oid"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "blobOid")))
                      :: Data.ProtoLens.FieldDescriptor File
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, path__field_descriptor),
                 (Data.ProtoLens.Tag 2, language__field_descriptor),
                 (Data.ProtoLens.Tag 3, symbols__field_descriptor),
                 (Data.ProtoLens.Tag 4, errors__field_descriptor),
                 (Data.ProtoLens.Tag 5, blobOid__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _File'_unknownFields
              (\ x__ y__ -> x__{_File'_unknownFields = y__})
        defMessage
          = File{_File'path = Data.ProtoLens.fieldDefault,
                 _File'language = Data.ProtoLens.fieldDefault, _File'symbols = [],
                 _File'errors = [], _File'blobOid = Data.ProtoLens.fieldDefault,
                 _File'_unknownFields = ([])}
instance Control.DeepSeq.NFData File where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_File'_unknownFields x__)
                (Control.DeepSeq.deepseq (_File'path x__)
                   (Control.DeepSeq.deepseq (_File'language x__)
                      (Control.DeepSeq.deepseq (_File'symbols x__)
                         (Control.DeepSeq.deepseq (_File'errors x__)
                            (Control.DeepSeq.deepseq (_File'blobOid x__) (()))))))
{- | Fields :

    * 'Proto.Semantic_Fields.term' @:: Lens' InsertedTerm Data.Text.Text@
    * 'Proto.Semantic_Fields.span' @:: Lens' InsertedTerm Span@
    * 'Proto.Semantic_Fields.maybe'span' @:: Lens' InsertedTerm (Prelude.Maybe Span)@
 -}
data InsertedTerm = InsertedTerm{_InsertedTerm'term ::
                                 !Data.Text.Text,
                                 _InsertedTerm'span :: !(Prelude.Maybe Span),
                                 _InsertedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show InsertedTerm where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' InsertedTerm "term" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _InsertedTerm'term
                 (\ x__ y__ -> x__{_InsertedTerm'term = y__}))
              Prelude.id
instance Lens.Labels.HasLens' InsertedTerm "span" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _InsertedTerm'span
                 (\ x__ y__ -> x__{_InsertedTerm'span = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' InsertedTerm "maybe'span"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _InsertedTerm'span
                 (\ x__ y__ -> x__{_InsertedTerm'span = y__}))
              Prelude.id
instance Data.ProtoLens.Message InsertedTerm where
        messageName _ = Data.Text.pack "github.semantic.InsertedTerm"
        fieldsByTag
          = let term__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "term"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "term")))
                      :: Data.ProtoLens.FieldDescriptor InsertedTerm
                span__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'span")))
                      :: Data.ProtoLens.FieldDescriptor InsertedTerm
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, term__field_descriptor),
                 (Data.ProtoLens.Tag 2, span__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _InsertedTerm'_unknownFields
              (\ x__ y__ -> x__{_InsertedTerm'_unknownFields = y__})
        defMessage
          = InsertedTerm{_InsertedTerm'term = Data.ProtoLens.fieldDefault,
                         _InsertedTerm'span = Prelude.Nothing,
                         _InsertedTerm'_unknownFields = ([])}
instance Control.DeepSeq.NFData InsertedTerm where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_InsertedTerm'_unknownFields x__)
                (Control.DeepSeq.deepseq (_InsertedTerm'term x__)
                   (Control.DeepSeq.deepseq (_InsertedTerm'span x__) (())))
{- | Fields :

    * 'Proto.Semantic_Fields.term' @:: Lens' MergedTerm Data.Text.Text@
    * 'Proto.Semantic_Fields.beforeSpan' @:: Lens' MergedTerm Span@
    * 'Proto.Semantic_Fields.maybe'beforeSpan' @:: Lens' MergedTerm (Prelude.Maybe Span)@
    * 'Proto.Semantic_Fields.afterSpan' @:: Lens' MergedTerm Span@
    * 'Proto.Semantic_Fields.maybe'afterSpan' @:: Lens' MergedTerm (Prelude.Maybe Span)@
 -}
data MergedTerm = MergedTerm{_MergedTerm'term :: !Data.Text.Text,
                             _MergedTerm'beforeSpan :: !(Prelude.Maybe Span),
                             _MergedTerm'afterSpan :: !(Prelude.Maybe Span),
                             _MergedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MergedTerm where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' MergedTerm "term" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MergedTerm'term
                 (\ x__ y__ -> x__{_MergedTerm'term = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MergedTerm "beforeSpan" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MergedTerm'beforeSpan
                 (\ x__ y__ -> x__{_MergedTerm'beforeSpan = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' MergedTerm "maybe'beforeSpan"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MergedTerm'beforeSpan
                 (\ x__ y__ -> x__{_MergedTerm'beforeSpan = y__}))
              Prelude.id
instance Lens.Labels.HasLens' MergedTerm "afterSpan" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MergedTerm'afterSpan
                 (\ x__ y__ -> x__{_MergedTerm'afterSpan = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' MergedTerm "maybe'afterSpan"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _MergedTerm'afterSpan
                 (\ x__ y__ -> x__{_MergedTerm'afterSpan = y__}))
              Prelude.id
instance Data.ProtoLens.Message MergedTerm where
        messageName _ = Data.Text.pack "github.semantic.MergedTerm"
        fieldsByTag
          = let term__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "term"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "term")))
                      :: Data.ProtoLens.FieldDescriptor MergedTerm
                beforeSpan__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "before_span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'beforeSpan")))
                      :: Data.ProtoLens.FieldDescriptor MergedTerm
                afterSpan__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "after_span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'afterSpan")))
                      :: Data.ProtoLens.FieldDescriptor MergedTerm
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, term__field_descriptor),
                 (Data.ProtoLens.Tag 2, beforeSpan__field_descriptor),
                 (Data.ProtoLens.Tag 3, afterSpan__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _MergedTerm'_unknownFields
              (\ x__ y__ -> x__{_MergedTerm'_unknownFields = y__})
        defMessage
          = MergedTerm{_MergedTerm'term = Data.ProtoLens.fieldDefault,
                       _MergedTerm'beforeSpan = Prelude.Nothing,
                       _MergedTerm'afterSpan = Prelude.Nothing,
                       _MergedTerm'_unknownFields = ([])}
instance Control.DeepSeq.NFData MergedTerm where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_MergedTerm'_unknownFields x__)
                (Control.DeepSeq.deepseq (_MergedTerm'term x__)
                   (Control.DeepSeq.deepseq (_MergedTerm'beforeSpan x__)
                      (Control.DeepSeq.deepseq (_MergedTerm'afterSpan x__) (()))))
{- | Fields :

    * 'Proto.Semantic_Fields.error' @:: Lens' ParseError Data.Text.Text@
 -}
data ParseError = ParseError{_ParseError'error :: !Data.Text.Text,
                             _ParseError'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseError where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ParseError "error" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseError'error
                 (\ x__ y__ -> x__{_ParseError'error = y__}))
              Prelude.id
instance Data.ProtoLens.Message ParseError where
        messageName _ = Data.Text.pack "github.semantic.ParseError"
        fieldsByTag
          = let error__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "error"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "error")))
                      :: Data.ProtoLens.FieldDescriptor ParseError
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, error__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ParseError'_unknownFields
              (\ x__ y__ -> x__{_ParseError'_unknownFields = y__})
        defMessage
          = ParseError{_ParseError'error = Data.ProtoLens.fieldDefault,
                       _ParseError'_unknownFields = ([])}
instance Control.DeepSeq.NFData ParseError where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_ParseError'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ParseError'error x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.path' @:: Lens' ParseTreeFileGraph Data.Text.Text@
    * 'Proto.Semantic_Fields.language' @:: Lens' ParseTreeFileGraph Data.Text.Text@
    * 'Proto.Semantic_Fields.vertices' @:: Lens' ParseTreeFileGraph [TermVertex]@
    * 'Proto.Semantic_Fields.edges' @:: Lens' ParseTreeFileGraph [TermEdge]@
    * 'Proto.Semantic_Fields.errors' @:: Lens' ParseTreeFileGraph [ParseError]@
 -}
data ParseTreeFileGraph = ParseTreeFileGraph{_ParseTreeFileGraph'path
                                             :: !Data.Text.Text,
                                             _ParseTreeFileGraph'language :: !Data.Text.Text,
                                             _ParseTreeFileGraph'vertices :: ![TermVertex],
                                             _ParseTreeFileGraph'edges :: ![TermEdge],
                                             _ParseTreeFileGraph'errors :: ![ParseError],
                                             _ParseTreeFileGraph'_unknownFields ::
                                             !Data.ProtoLens.FieldSet}
                            deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeFileGraph where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ParseTreeFileGraph "path"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeFileGraph'path
                 (\ x__ y__ -> x__{_ParseTreeFileGraph'path = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ParseTreeFileGraph "language"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeFileGraph'language
                 (\ x__ y__ -> x__{_ParseTreeFileGraph'language = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ParseTreeFileGraph "vertices"
           ([TermVertex])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeFileGraph'vertices
                 (\ x__ y__ -> x__{_ParseTreeFileGraph'vertices = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ParseTreeFileGraph "edges"
           ([TermEdge])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeFileGraph'edges
                 (\ x__ y__ -> x__{_ParseTreeFileGraph'edges = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ParseTreeFileGraph "errors"
           ([ParseError])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeFileGraph'errors
                 (\ x__ y__ -> x__{_ParseTreeFileGraph'errors = y__}))
              Prelude.id
instance Data.ProtoLens.Message ParseTreeFileGraph where
        messageName _ = Data.Text.pack "github.semantic.ParseTreeFileGraph"
        fieldsByTag
          = let path__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "path"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "path")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
                language__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "language"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "language")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
                vertices__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vertices"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TermVertex)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "vertices")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
                edges__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "edges"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TermEdge)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "edges")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
                errors__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "errors"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ParseError)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "errors")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, path__field_descriptor),
                 (Data.ProtoLens.Tag 2, language__field_descriptor),
                 (Data.ProtoLens.Tag 3, vertices__field_descriptor),
                 (Data.ProtoLens.Tag 4, edges__field_descriptor),
                 (Data.ProtoLens.Tag 5, errors__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ParseTreeFileGraph'_unknownFields
              (\ x__ y__ -> x__{_ParseTreeFileGraph'_unknownFields = y__})
        defMessage
          = ParseTreeFileGraph{_ParseTreeFileGraph'path =
                                 Data.ProtoLens.fieldDefault,
                               _ParseTreeFileGraph'language = Data.ProtoLens.fieldDefault,
                               _ParseTreeFileGraph'vertices = [], _ParseTreeFileGraph'edges = [],
                               _ParseTreeFileGraph'errors = [],
                               _ParseTreeFileGraph'_unknownFields = ([])}
instance Control.DeepSeq.NFData ParseTreeFileGraph where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_ParseTreeFileGraph'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ParseTreeFileGraph'path x__)
                   (Control.DeepSeq.deepseq (_ParseTreeFileGraph'language x__)
                      (Control.DeepSeq.deepseq (_ParseTreeFileGraph'vertices x__)
                         (Control.DeepSeq.deepseq (_ParseTreeFileGraph'edges x__)
                            (Control.DeepSeq.deepseq (_ParseTreeFileGraph'errors x__) (()))))))
{- | Fields :

    * 'Proto.Semantic_Fields.files' @:: Lens' ParseTreeGraphResponse [ParseTreeFileGraph]@
 -}
data ParseTreeGraphResponse = ParseTreeGraphResponse{_ParseTreeGraphResponse'files
                                                     :: ![ParseTreeFileGraph],
                                                     _ParseTreeGraphResponse'_unknownFields ::
                                                     !Data.ProtoLens.FieldSet}
                                deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeGraphResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ParseTreeGraphResponse "files"
           ([ParseTreeFileGraph])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeGraphResponse'files
                 (\ x__ y__ -> x__{_ParseTreeGraphResponse'files = y__}))
              Prelude.id
instance Data.ProtoLens.Message ParseTreeGraphResponse where
        messageName _
          = Data.Text.pack "github.semantic.ParseTreeGraphResponse"
        fieldsByTag
          = let files__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "files"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor ParseTreeFileGraph)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "files")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeGraphResponse
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _ParseTreeGraphResponse'_unknownFields
              (\ x__ y__ -> x__{_ParseTreeGraphResponse'_unknownFields = y__})
        defMessage
          = ParseTreeGraphResponse{_ParseTreeGraphResponse'files = [],
                                   _ParseTreeGraphResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData ParseTreeGraphResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_ParseTreeGraphResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ParseTreeGraphResponse'files x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.blobs' @:: Lens' ParseTreeRequest [Blob]@
 -}
data ParseTreeRequest = ParseTreeRequest{_ParseTreeRequest'blobs ::
                                         ![Blob],
                                         _ParseTreeRequest'_unknownFields ::
                                         !Data.ProtoLens.FieldSet}
                          deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ParseTreeRequest "blobs" ([Blob])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeRequest'blobs
                 (\ x__ y__ -> x__{_ParseTreeRequest'blobs = y__}))
              Prelude.id
instance Data.ProtoLens.Message ParseTreeRequest where
        messageName _ = Data.Text.pack "github.semantic.ParseTreeRequest"
        fieldsByTag
          = let blobs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "blobs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Blob)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "blobs")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeRequest
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, blobs__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ParseTreeRequest'_unknownFields
              (\ x__ y__ -> x__{_ParseTreeRequest'_unknownFields = y__})
        defMessage
          = ParseTreeRequest{_ParseTreeRequest'blobs = [],
                             _ParseTreeRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData ParseTreeRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_ParseTreeRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ParseTreeRequest'blobs x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.files' @:: Lens' ParseTreeSymbolResponse [File]@
 -}
data ParseTreeSymbolResponse = ParseTreeSymbolResponse{_ParseTreeSymbolResponse'files
                                                       :: ![File],
                                                       _ParseTreeSymbolResponse'_unknownFields ::
                                                       !Data.ProtoLens.FieldSet}
                                 deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeSymbolResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ParseTreeSymbolResponse "files"
           ([File])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ParseTreeSymbolResponse'files
                 (\ x__ y__ -> x__{_ParseTreeSymbolResponse'files = y__}))
              Prelude.id
instance Data.ProtoLens.Message ParseTreeSymbolResponse where
        messageName _
          = Data.Text.pack "github.semantic.ParseTreeSymbolResponse"
        fieldsByTag
          = let files__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "files"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor File)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "files")))
                      :: Data.ProtoLens.FieldDescriptor ParseTreeSymbolResponse
              in
              Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens
              _ParseTreeSymbolResponse'_unknownFields
              (\ x__ y__ -> x__{_ParseTreeSymbolResponse'_unknownFields = y__})
        defMessage
          = ParseTreeSymbolResponse{_ParseTreeSymbolResponse'files = [],
                                    _ParseTreeSymbolResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData ParseTreeSymbolResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq
                (_ParseTreeSymbolResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ParseTreeSymbolResponse'files x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.service' @:: Lens' PingRequest Data.Text.Text@
 -}
data PingRequest = PingRequest{_PingRequest'service ::
                               !Data.Text.Text,
                               _PingRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
                     deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PingRequest where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' PingRequest "service"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PingRequest'service
                 (\ x__ y__ -> x__{_PingRequest'service = y__}))
              Prelude.id
instance Data.ProtoLens.Message PingRequest where
        messageName _ = Data.Text.pack "github.semantic.PingRequest"
        fieldsByTag
          = let service__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "service"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "service")))
                      :: Data.ProtoLens.FieldDescriptor PingRequest
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, service__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _PingRequest'_unknownFields
              (\ x__ y__ -> x__{_PingRequest'_unknownFields = y__})
        defMessage
          = PingRequest{_PingRequest'service = Data.ProtoLens.fieldDefault,
                        _PingRequest'_unknownFields = ([])}
instance Control.DeepSeq.NFData PingRequest where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_PingRequest'_unknownFields x__)
                (Control.DeepSeq.deepseq (_PingRequest'service x__) (()))
{- | Fields :

    * 'Proto.Semantic_Fields.status' @:: Lens' PingResponse Data.Text.Text@
    * 'Proto.Semantic_Fields.hostname' @:: Lens' PingResponse Data.Text.Text@
    * 'Proto.Semantic_Fields.timestamp' @:: Lens' PingResponse Data.Text.Text@
    * 'Proto.Semantic_Fields.sha' @:: Lens' PingResponse Data.Text.Text@
 -}
data PingResponse = PingResponse{_PingResponse'status ::
                                 !Data.Text.Text,
                                 _PingResponse'hostname :: !Data.Text.Text,
                                 _PingResponse'timestamp :: !Data.Text.Text,
                                 _PingResponse'sha :: !Data.Text.Text,
                                 _PingResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PingResponse where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' PingResponse "status"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PingResponse'status
                 (\ x__ y__ -> x__{_PingResponse'status = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PingResponse "hostname"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PingResponse'hostname
                 (\ x__ y__ -> x__{_PingResponse'hostname = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PingResponse "timestamp"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PingResponse'timestamp
                 (\ x__ y__ -> x__{_PingResponse'timestamp = y__}))
              Prelude.id
instance Lens.Labels.HasLens' PingResponse "sha" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _PingResponse'sha
                 (\ x__ y__ -> x__{_PingResponse'sha = y__}))
              Prelude.id
instance Data.ProtoLens.Message PingResponse where
        messageName _ = Data.Text.pack "github.semantic.PingResponse"
        fieldsByTag
          = let status__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "status"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "status")))
                      :: Data.ProtoLens.FieldDescriptor PingResponse
                hostname__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "hostname"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "hostname")))
                      :: Data.ProtoLens.FieldDescriptor PingResponse
                timestamp__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "timestamp"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "timestamp")))
                      :: Data.ProtoLens.FieldDescriptor PingResponse
                sha__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "sha"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "sha")))
                      :: Data.ProtoLens.FieldDescriptor PingResponse
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, status__field_descriptor),
                 (Data.ProtoLens.Tag 2, hostname__field_descriptor),
                 (Data.ProtoLens.Tag 3, timestamp__field_descriptor),
                 (Data.ProtoLens.Tag 4, sha__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _PingResponse'_unknownFields
              (\ x__ y__ -> x__{_PingResponse'_unknownFields = y__})
        defMessage
          = PingResponse{_PingResponse'status = Data.ProtoLens.fieldDefault,
                         _PingResponse'hostname = Data.ProtoLens.fieldDefault,
                         _PingResponse'timestamp = Data.ProtoLens.fieldDefault,
                         _PingResponse'sha = Data.ProtoLens.fieldDefault,
                         _PingResponse'_unknownFields = ([])}
instance Control.DeepSeq.NFData PingResponse where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_PingResponse'_unknownFields x__)
                (Control.DeepSeq.deepseq (_PingResponse'status x__)
                   (Control.DeepSeq.deepseq (_PingResponse'hostname x__)
                      (Control.DeepSeq.deepseq (_PingResponse'timestamp x__)
                         (Control.DeepSeq.deepseq (_PingResponse'sha x__) (())))))
{- | Fields :

    * 'Proto.Semantic_Fields.line' @:: Lens' Position Data.Int.Int32@
    * 'Proto.Semantic_Fields.column' @:: Lens' Position Data.Int.Int32@
 -}
data Position = Position{_Position'line :: !Data.Int.Int32,
                         _Position'column :: !Data.Int.Int32,
                         _Position'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Position where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Position "line" (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Position'line
                 (\ x__ y__ -> x__{_Position'line = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Position "column" (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Position'column
                 (\ x__ y__ -> x__{_Position'column = y__}))
              Prelude.id
instance Data.ProtoLens.Message Position where
        messageName _ = Data.Text.pack "github.semantic.Position"
        fieldsByTag
          = let line__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "line"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "line")))
                      :: Data.ProtoLens.FieldDescriptor Position
                column__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "column"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "column")))
                      :: Data.ProtoLens.FieldDescriptor Position
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, line__field_descriptor),
                 (Data.ProtoLens.Tag 2, column__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Position'_unknownFields
              (\ x__ y__ -> x__{_Position'_unknownFields = y__})
        defMessage
          = Position{_Position'line = Data.ProtoLens.fieldDefault,
                     _Position'column = Data.ProtoLens.fieldDefault,
                     _Position'_unknownFields = ([])}
instance Control.DeepSeq.NFData Position where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Position'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Position'line x__)
                   (Control.DeepSeq.deepseq (_Position'column x__) (())))
{- | Fields :

    * 'Proto.Semantic_Fields.beforeTerm' @:: Lens' ReplacedTerm Data.Text.Text@
    * 'Proto.Semantic_Fields.beforeSpan' @:: Lens' ReplacedTerm Span@
    * 'Proto.Semantic_Fields.maybe'beforeSpan' @:: Lens' ReplacedTerm (Prelude.Maybe Span)@
    * 'Proto.Semantic_Fields.afterTerm' @:: Lens' ReplacedTerm Data.Text.Text@
    * 'Proto.Semantic_Fields.afterSpan' @:: Lens' ReplacedTerm Span@
    * 'Proto.Semantic_Fields.maybe'afterSpan' @:: Lens' ReplacedTerm (Prelude.Maybe Span)@
 -}
data ReplacedTerm = ReplacedTerm{_ReplacedTerm'beforeTerm ::
                                 !Data.Text.Text,
                                 _ReplacedTerm'beforeSpan :: !(Prelude.Maybe Span),
                                 _ReplacedTerm'afterTerm :: !Data.Text.Text,
                                 _ReplacedTerm'afterSpan :: !(Prelude.Maybe Span),
                                 _ReplacedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
                      deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReplacedTerm where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' ReplacedTerm "beforeTerm"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ReplacedTerm'beforeTerm
                 (\ x__ y__ -> x__{_ReplacedTerm'beforeTerm = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ReplacedTerm "beforeSpan" (Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ReplacedTerm'beforeSpan
                 (\ x__ y__ -> x__{_ReplacedTerm'beforeSpan = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' ReplacedTerm "maybe'beforeSpan"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ReplacedTerm'beforeSpan
                 (\ x__ y__ -> x__{_ReplacedTerm'beforeSpan = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ReplacedTerm "afterTerm"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ReplacedTerm'afterTerm
                 (\ x__ y__ -> x__{_ReplacedTerm'afterTerm = y__}))
              Prelude.id
instance Lens.Labels.HasLens' ReplacedTerm "afterSpan" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ReplacedTerm'afterSpan
                 (\ x__ y__ -> x__{_ReplacedTerm'afterSpan = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' ReplacedTerm "maybe'afterSpan"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ReplacedTerm'afterSpan
                 (\ x__ y__ -> x__{_ReplacedTerm'afterSpan = y__}))
              Prelude.id
instance Data.ProtoLens.Message ReplacedTerm where
        messageName _ = Data.Text.pack "github.semantic.ReplacedTerm"
        fieldsByTag
          = let beforeTerm__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "before_term"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "beforeTerm")))
                      :: Data.ProtoLens.FieldDescriptor ReplacedTerm
                beforeSpan__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "before_span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'beforeSpan")))
                      :: Data.ProtoLens.FieldDescriptor ReplacedTerm
                afterTerm__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "after_term"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "afterTerm")))
                      :: Data.ProtoLens.FieldDescriptor ReplacedTerm
                afterSpan__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "after_span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'afterSpan")))
                      :: Data.ProtoLens.FieldDescriptor ReplacedTerm
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, beforeTerm__field_descriptor),
                 (Data.ProtoLens.Tag 2, beforeSpan__field_descriptor),
                 (Data.ProtoLens.Tag 3, afterTerm__field_descriptor),
                 (Data.ProtoLens.Tag 4, afterSpan__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _ReplacedTerm'_unknownFields
              (\ x__ y__ -> x__{_ReplacedTerm'_unknownFields = y__})
        defMessage
          = ReplacedTerm{_ReplacedTerm'beforeTerm =
                           Data.ProtoLens.fieldDefault,
                         _ReplacedTerm'beforeSpan = Prelude.Nothing,
                         _ReplacedTerm'afterTerm = Data.ProtoLens.fieldDefault,
                         _ReplacedTerm'afterSpan = Prelude.Nothing,
                         _ReplacedTerm'_unknownFields = ([])}
instance Control.DeepSeq.NFData ReplacedTerm where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_ReplacedTerm'_unknownFields x__)
                (Control.DeepSeq.deepseq (_ReplacedTerm'beforeTerm x__)
                   (Control.DeepSeq.deepseq (_ReplacedTerm'beforeSpan x__)
                      (Control.DeepSeq.deepseq (_ReplacedTerm'afterTerm x__)
                         (Control.DeepSeq.deepseq (_ReplacedTerm'afterSpan x__) (())))))
{- | Fields :

    * 'Proto.Semantic_Fields.start' @:: Lens' Span Position@
    * 'Proto.Semantic_Fields.maybe'start' @:: Lens' Span (Prelude.Maybe Position)@
    * 'Proto.Semantic_Fields.end' @:: Lens' Span Position@
    * 'Proto.Semantic_Fields.maybe'end' @:: Lens' Span (Prelude.Maybe Position)@
 -}
data Span = Span{_Span'start :: !(Prelude.Maybe Position),
                 _Span'end :: !(Prelude.Maybe Position),
                 _Span'_unknownFields :: !Data.ProtoLens.FieldSet}
              deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Span where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Span "start" (Position) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Span'start
                 (\ x__ y__ -> x__{_Span'start = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' Span "maybe'start"
           (Prelude.Maybe Position)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Span'start
                 (\ x__ y__ -> x__{_Span'start = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Span "end" (Position) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Span'end
                 (\ x__ y__ -> x__{_Span'end = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' Span "maybe'end"
           (Prelude.Maybe Position)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Span'end
                 (\ x__ y__ -> x__{_Span'end = y__}))
              Prelude.id
instance Data.ProtoLens.Message Span where
        messageName _ = Data.Text.pack "github.semantic.Span"
        fieldsByTag
          = let start__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "start"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Position)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'start")))
                      :: Data.ProtoLens.FieldDescriptor Span
                end__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "end"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Position)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'end")))
                      :: Data.ProtoLens.FieldDescriptor Span
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, start__field_descriptor),
                 (Data.ProtoLens.Tag 2, end__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Span'_unknownFields
              (\ x__ y__ -> x__{_Span'_unknownFields = y__})
        defMessage
          = Span{_Span'start = Prelude.Nothing, _Span'end = Prelude.Nothing,
                 _Span'_unknownFields = ([])}
instance Control.DeepSeq.NFData Span where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Span'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Span'start x__)
                   (Control.DeepSeq.deepseq (_Span'end x__) (())))
{- | Fields :

    * 'Proto.Semantic_Fields.symbol' @:: Lens' Symbol Data.Text.Text@
    * 'Proto.Semantic_Fields.kind' @:: Lens' Symbol Data.Text.Text@
    * 'Proto.Semantic_Fields.line' @:: Lens' Symbol Data.Text.Text@
    * 'Proto.Semantic_Fields.span' @:: Lens' Symbol Span@
    * 'Proto.Semantic_Fields.maybe'span' @:: Lens' Symbol (Prelude.Maybe Span)@
    * 'Proto.Semantic_Fields.docs' @:: Lens' Symbol Docstring@
    * 'Proto.Semantic_Fields.maybe'docs' @:: Lens' Symbol (Prelude.Maybe Docstring)@
 -}
data Symbol = Symbol{_Symbol'symbol :: !Data.Text.Text,
                     _Symbol'kind :: !Data.Text.Text, _Symbol'line :: !Data.Text.Text,
                     _Symbol'span :: !(Prelude.Maybe Span),
                     _Symbol'docs :: !(Prelude.Maybe Docstring),
                     _Symbol'_unknownFields :: !Data.ProtoLens.FieldSet}
                deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Symbol where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' Symbol "symbol" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Symbol'symbol
                 (\ x__ y__ -> x__{_Symbol'symbol = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Symbol "kind" (Data.Text.Text) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Symbol'kind
                 (\ x__ y__ -> x__{_Symbol'kind = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Symbol "line" (Data.Text.Text) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Symbol'line
                 (\ x__ y__ -> x__{_Symbol'line = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Symbol "span" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Symbol'span
                 (\ x__ y__ -> x__{_Symbol'span = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' Symbol "maybe'span"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Symbol'span
                 (\ x__ y__ -> x__{_Symbol'span = y__}))
              Prelude.id
instance Lens.Labels.HasLens' Symbol "docs" (Docstring) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Symbol'docs
                 (\ x__ y__ -> x__{_Symbol'docs = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' Symbol "maybe'docs"
           (Prelude.Maybe Docstring)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _Symbol'docs
                 (\ x__ y__ -> x__{_Symbol'docs = y__}))
              Prelude.id
instance Data.ProtoLens.Message Symbol where
        messageName _ = Data.Text.pack "github.semantic.Symbol"
        fieldsByTag
          = let symbol__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "symbol"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "symbol")))
                      :: Data.ProtoLens.FieldDescriptor Symbol
                kind__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "kind"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "kind")))
                      :: Data.ProtoLens.FieldDescriptor Symbol
                line__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "line"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "line")))
                      :: Data.ProtoLens.FieldDescriptor Symbol
                span__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'span")))
                      :: Data.ProtoLens.FieldDescriptor Symbol
                docs__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "docs"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Docstring)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'docs")))
                      :: Data.ProtoLens.FieldDescriptor Symbol
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
                 (Data.ProtoLens.Tag 2, kind__field_descriptor),
                 (Data.ProtoLens.Tag 3, line__field_descriptor),
                 (Data.ProtoLens.Tag 4, span__field_descriptor),
                 (Data.ProtoLens.Tag 5, docs__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _Symbol'_unknownFields
              (\ x__ y__ -> x__{_Symbol'_unknownFields = y__})
        defMessage
          = Symbol{_Symbol'symbol = Data.ProtoLens.fieldDefault,
                   _Symbol'kind = Data.ProtoLens.fieldDefault,
                   _Symbol'line = Data.ProtoLens.fieldDefault,
                   _Symbol'span = Prelude.Nothing, _Symbol'docs = Prelude.Nothing,
                   _Symbol'_unknownFields = ([])}
instance Control.DeepSeq.NFData Symbol where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_Symbol'_unknownFields x__)
                (Control.DeepSeq.deepseq (_Symbol'symbol x__)
                   (Control.DeepSeq.deepseq (_Symbol'kind x__)
                      (Control.DeepSeq.deepseq (_Symbol'line x__)
                         (Control.DeepSeq.deepseq (_Symbol'span x__)
                            (Control.DeepSeq.deepseq (_Symbol'docs x__) (()))))))
{- | Fields :

    * 'Proto.Semantic_Fields.category' @:: Lens' TOCSummaryChange Data.Text.Text@
    * 'Proto.Semantic_Fields.term' @:: Lens' TOCSummaryChange Data.Text.Text@
    * 'Proto.Semantic_Fields.span' @:: Lens' TOCSummaryChange Span@
    * 'Proto.Semantic_Fields.maybe'span' @:: Lens' TOCSummaryChange (Prelude.Maybe Span)@
    * 'Proto.Semantic_Fields.changeType' @:: Lens' TOCSummaryChange ChangeType@
 -}
data TOCSummaryChange = TOCSummaryChange{_TOCSummaryChange'category
                                         :: !Data.Text.Text,
                                         _TOCSummaryChange'term :: !Data.Text.Text,
                                         _TOCSummaryChange'span :: !(Prelude.Maybe Span),
                                         _TOCSummaryChange'changeType :: !ChangeType,
                                         _TOCSummaryChange'_unknownFields ::
                                         !Data.ProtoLens.FieldSet}
                          deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TOCSummaryChange where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' TOCSummaryChange "category"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryChange'category
                 (\ x__ y__ -> x__{_TOCSummaryChange'category = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TOCSummaryChange "term"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryChange'term
                 (\ x__ y__ -> x__{_TOCSummaryChange'term = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TOCSummaryChange "span" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryChange'span
                 (\ x__ y__ -> x__{_TOCSummaryChange'span = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' TOCSummaryChange "maybe'span"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryChange'span
                 (\ x__ y__ -> x__{_TOCSummaryChange'span = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TOCSummaryChange "changeType"
           (ChangeType)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryChange'changeType
                 (\ x__ y__ -> x__{_TOCSummaryChange'changeType = y__}))
              Prelude.id
instance Data.ProtoLens.Message TOCSummaryChange where
        messageName _ = Data.Text.pack "github.semantic.TOCSummaryChange"
        fieldsByTag
          = let category__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "category"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "category")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryChange
                term__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "term"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "term")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryChange
                span__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'span")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryChange
                changeType__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "change_type"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                         Data.ProtoLens.FieldTypeDescriptor ChangeType)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "changeType")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryChange
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, category__field_descriptor),
                 (Data.ProtoLens.Tag 2, term__field_descriptor),
                 (Data.ProtoLens.Tag 3, span__field_descriptor),
                 (Data.ProtoLens.Tag 4, changeType__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TOCSummaryChange'_unknownFields
              (\ x__ y__ -> x__{_TOCSummaryChange'_unknownFields = y__})
        defMessage
          = TOCSummaryChange{_TOCSummaryChange'category =
                               Data.ProtoLens.fieldDefault,
                             _TOCSummaryChange'term = Data.ProtoLens.fieldDefault,
                             _TOCSummaryChange'span = Prelude.Nothing,
                             _TOCSummaryChange'changeType = Data.ProtoLens.fieldDefault,
                             _TOCSummaryChange'_unknownFields = ([])}
instance Control.DeepSeq.NFData TOCSummaryChange where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_TOCSummaryChange'_unknownFields x__)
                (Control.DeepSeq.deepseq (_TOCSummaryChange'category x__)
                   (Control.DeepSeq.deepseq (_TOCSummaryChange'term x__)
                      (Control.DeepSeq.deepseq (_TOCSummaryChange'span x__)
                         (Control.DeepSeq.deepseq (_TOCSummaryChange'changeType x__)
                            (())))))
{- | Fields :

    * 'Proto.Semantic_Fields.error' @:: Lens' TOCSummaryError Data.Text.Text@
    * 'Proto.Semantic_Fields.span' @:: Lens' TOCSummaryError Span@
    * 'Proto.Semantic_Fields.maybe'span' @:: Lens' TOCSummaryError (Prelude.Maybe Span)@
 -}
data TOCSummaryError = TOCSummaryError{_TOCSummaryError'error ::
                                       !Data.Text.Text,
                                       _TOCSummaryError'span :: !(Prelude.Maybe Span),
                                       _TOCSummaryError'_unknownFields :: !Data.ProtoLens.FieldSet}
                         deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TOCSummaryError where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' TOCSummaryError "error"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryError'error
                 (\ x__ y__ -> x__{_TOCSummaryError'error = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TOCSummaryError "span" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryError'span
                 (\ x__ y__ -> x__{_TOCSummaryError'span = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' TOCSummaryError "maybe'span"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryError'span
                 (\ x__ y__ -> x__{_TOCSummaryError'span = y__}))
              Prelude.id
instance Data.ProtoLens.Message TOCSummaryError where
        messageName _ = Data.Text.pack "github.semantic.TOCSummaryError"
        fieldsByTag
          = let error__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "error"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "error")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryError
                span__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'span")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryError
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, error__field_descriptor),
                 (Data.ProtoLens.Tag 2, span__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TOCSummaryError'_unknownFields
              (\ x__ y__ -> x__{_TOCSummaryError'_unknownFields = y__})
        defMessage
          = TOCSummaryError{_TOCSummaryError'error =
                              Data.ProtoLens.fieldDefault,
                            _TOCSummaryError'span = Prelude.Nothing,
                            _TOCSummaryError'_unknownFields = ([])}
instance Control.DeepSeq.NFData TOCSummaryError where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_TOCSummaryError'_unknownFields x__)
                (Control.DeepSeq.deepseq (_TOCSummaryError'error x__)
                   (Control.DeepSeq.deepseq (_TOCSummaryError'span x__) (())))
{- | Fields :

    * 'Proto.Semantic_Fields.path' @:: Lens' TOCSummaryFile Data.Text.Text@
    * 'Proto.Semantic_Fields.language' @:: Lens' TOCSummaryFile Data.Text.Text@
    * 'Proto.Semantic_Fields.changes' @:: Lens' TOCSummaryFile [TOCSummaryChange]@
    * 'Proto.Semantic_Fields.errors' @:: Lens' TOCSummaryFile [TOCSummaryError]@
 -}
data TOCSummaryFile = TOCSummaryFile{_TOCSummaryFile'path ::
                                     !Data.Text.Text,
                                     _TOCSummaryFile'language :: !Data.Text.Text,
                                     _TOCSummaryFile'changes :: ![TOCSummaryChange],
                                     _TOCSummaryFile'errors :: ![TOCSummaryError],
                                     _TOCSummaryFile'_unknownFields :: !Data.ProtoLens.FieldSet}
                        deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TOCSummaryFile where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' TOCSummaryFile "path"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryFile'path
                 (\ x__ y__ -> x__{_TOCSummaryFile'path = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TOCSummaryFile "language"
           (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryFile'language
                 (\ x__ y__ -> x__{_TOCSummaryFile'language = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TOCSummaryFile "changes"
           ([TOCSummaryChange])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryFile'changes
                 (\ x__ y__ -> x__{_TOCSummaryFile'changes = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TOCSummaryFile "errors"
           ([TOCSummaryError])
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TOCSummaryFile'errors
                 (\ x__ y__ -> x__{_TOCSummaryFile'errors = y__}))
              Prelude.id
instance Data.ProtoLens.Message TOCSummaryFile where
        messageName _ = Data.Text.pack "github.semantic.TOCSummaryFile"
        fieldsByTag
          = let path__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "path"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "path")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryFile
                language__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "language"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "language")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryFile
                changes__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "changes"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TOCSummaryChange)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "changes")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryFile
                errors__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "errors"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor TOCSummaryError)
                      (Data.ProtoLens.RepeatedField Data.ProtoLens.Unpacked
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "errors")))
                      :: Data.ProtoLens.FieldDescriptor TOCSummaryFile
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, path__field_descriptor),
                 (Data.ProtoLens.Tag 2, language__field_descriptor),
                 (Data.ProtoLens.Tag 3, changes__field_descriptor),
                 (Data.ProtoLens.Tag 4, errors__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TOCSummaryFile'_unknownFields
              (\ x__ y__ -> x__{_TOCSummaryFile'_unknownFields = y__})
        defMessage
          = TOCSummaryFile{_TOCSummaryFile'path =
                             Data.ProtoLens.fieldDefault,
                           _TOCSummaryFile'language = Data.ProtoLens.fieldDefault,
                           _TOCSummaryFile'changes = [], _TOCSummaryFile'errors = [],
                           _TOCSummaryFile'_unknownFields = ([])}
instance Control.DeepSeq.NFData TOCSummaryFile where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_TOCSummaryFile'_unknownFields x__)
                (Control.DeepSeq.deepseq (_TOCSummaryFile'path x__)
                   (Control.DeepSeq.deepseq (_TOCSummaryFile'language x__)
                      (Control.DeepSeq.deepseq (_TOCSummaryFile'changes x__)
                         (Control.DeepSeq.deepseq (_TOCSummaryFile'errors x__) (())))))
{- | Fields :

    * 'Proto.Semantic_Fields.source' @:: Lens' TermEdge Data.Int.Int32@
    * 'Proto.Semantic_Fields.target' @:: Lens' TermEdge Data.Int.Int32@
 -}
data TermEdge = TermEdge{_TermEdge'source :: !Data.Int.Int32,
                         _TermEdge'target :: !Data.Int.Int32,
                         _TermEdge'_unknownFields :: !Data.ProtoLens.FieldSet}
                  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TermEdge where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' TermEdge "source" (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TermEdge'source
                 (\ x__ y__ -> x__{_TermEdge'source = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TermEdge "target" (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TermEdge'target
                 (\ x__ y__ -> x__{_TermEdge'target = y__}))
              Prelude.id
instance Data.ProtoLens.Message TermEdge where
        messageName _ = Data.Text.pack "github.semantic.TermEdge"
        fieldsByTag
          = let source__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "source"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "source")))
                      :: Data.ProtoLens.FieldDescriptor TermEdge
                target__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "target"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "target")))
                      :: Data.ProtoLens.FieldDescriptor TermEdge
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, source__field_descriptor),
                 (Data.ProtoLens.Tag 2, target__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TermEdge'_unknownFields
              (\ x__ y__ -> x__{_TermEdge'_unknownFields = y__})
        defMessage
          = TermEdge{_TermEdge'source = Data.ProtoLens.fieldDefault,
                     _TermEdge'target = Data.ProtoLens.fieldDefault,
                     _TermEdge'_unknownFields = ([])}
instance Control.DeepSeq.NFData TermEdge where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_TermEdge'_unknownFields x__)
                (Control.DeepSeq.deepseq (_TermEdge'source x__)
                   (Control.DeepSeq.deepseq (_TermEdge'target x__) (())))
{- | Fields :

    * 'Proto.Semantic_Fields.vertexId' @:: Lens' TermVertex Data.Int.Int32@
    * 'Proto.Semantic_Fields.term' @:: Lens' TermVertex Data.Text.Text@
    * 'Proto.Semantic_Fields.span' @:: Lens' TermVertex Span@
    * 'Proto.Semantic_Fields.maybe'span' @:: Lens' TermVertex (Prelude.Maybe Span)@
 -}
data TermVertex = TermVertex{_TermVertex'vertexId ::
                             !Data.Int.Int32,
                             _TermVertex'term :: !Data.Text.Text,
                             _TermVertex'span :: !(Prelude.Maybe Span),
                             _TermVertex'_unknownFields :: !Data.ProtoLens.FieldSet}
                    deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TermVertex where
        showsPrec _ __x __s
          = Prelude.showChar '{'
              (Prelude.showString (Data.ProtoLens.showMessageShort __x)
                 (Prelude.showChar '}' __s))
instance Lens.Labels.HasLens' TermVertex "vertexId"
           (Data.Int.Int32)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TermVertex'vertexId
                 (\ x__ y__ -> x__{_TermVertex'vertexId = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TermVertex "term" (Data.Text.Text)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TermVertex'term
                 (\ x__ y__ -> x__{_TermVertex'term = y__}))
              Prelude.id
instance Lens.Labels.HasLens' TermVertex "span" (Span) where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TermVertex'span
                 (\ x__ y__ -> x__{_TermVertex'span = y__}))
              (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Lens.Labels.HasLens' TermVertex "maybe'span"
           (Prelude.Maybe Span)
         where
        lensOf' _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _TermVertex'span
                 (\ x__ y__ -> x__{_TermVertex'span = y__}))
              Prelude.id
instance Data.ProtoLens.Message TermVertex where
        messageName _ = Data.Text.pack "github.semantic.TermVertex"
        fieldsByTag
          = let vertexId__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "vertex_id"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "vertexId")))
                      :: Data.ProtoLens.FieldDescriptor TermVertex
                term__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "term"
                      (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "term")))
                      :: Data.ProtoLens.FieldDescriptor TermVertex
                span__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "span"
                      (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                         Data.ProtoLens.FieldTypeDescriptor Span)
                      (Data.ProtoLens.OptionalField
                         (Lens.Labels.lensOf'
                            ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "maybe'span")))
                      :: Data.ProtoLens.FieldDescriptor TermVertex
              in
              Data.Map.fromList
                [(Data.ProtoLens.Tag 1, vertexId__field_descriptor),
                 (Data.ProtoLens.Tag 2, term__field_descriptor),
                 (Data.ProtoLens.Tag 3, span__field_descriptor)]
        unknownFields
          = Lens.Family2.Unchecked.lens _TermVertex'_unknownFields
              (\ x__ y__ -> x__{_TermVertex'_unknownFields = y__})
        defMessage
          = TermVertex{_TermVertex'vertexId = Data.ProtoLens.fieldDefault,
                       _TermVertex'term = Data.ProtoLens.fieldDefault,
                       _TermVertex'span = Prelude.Nothing,
                       _TermVertex'_unknownFields = ([])}
instance Control.DeepSeq.NFData TermVertex where
        rnf
          = \ x__ ->
              Control.DeepSeq.deepseq (_TermVertex'_unknownFields x__)
                (Control.DeepSeq.deepseq (_TermVertex'vertexId x__)
                   (Control.DeepSeq.deepseq (_TermVertex'term x__)
                      (Control.DeepSeq.deepseq (_TermVertex'span x__) (()))))
