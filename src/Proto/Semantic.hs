{- This file was auto-generated from semantic.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Semantic (
        Blob(), BlobPair(), ChangeType(..), ChangeType(),
        ChangeType'UnrecognizedValue, DeletedTerm(), DiffTreeEdge(),
        DiffTreeFileGraph(), DiffTreeGraphResponse(), DiffTreeRequest(),
        DiffTreeTOCResponse(), DiffTreeVertex(),
        DiffTreeVertex'DiffTerm(..), _DiffTreeVertex'Deleted,
        _DiffTreeVertex'Inserted, _DiffTreeVertex'Replaced,
        _DiffTreeVertex'Merged, Docstring(), File(), InsertedTerm(),
        MergedTerm(), ParseError(), ParseTreeFileGraph(),
        ParseTreeGraphResponse(), ParseTreeRequest(),
        ParseTreeSymbolResponse(), PingRequest(), PingResponse(),
        Position(), ReplacedTerm(), Span(), StackGraphFile(),
        StackGraphNode(), StackGraphNode'NodeType(..),
        StackGraphNode'NodeType(),
        StackGraphNode'NodeType'UnrecognizedValue, StackGraphPath(),
        StackGraphRequest(), StackGraphResponse(), Symbol(),
        TOCSummaryChange(), TOCSummaryError(), TOCSummaryFile(),
        TermEdge(), TermVertex()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
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
{- | Fields :
     
         * 'Proto.Semantic_Fields.content' @:: Lens' Blob Data.Text.Text@
         * 'Proto.Semantic_Fields.path' @:: Lens' Blob Data.Text.Text@
         * 'Proto.Semantic_Fields.language' @:: Lens' Blob Data.Text.Text@ -}
data Blob
  = Blob'_constructor {_Blob'content :: !Data.Text.Text,
                       _Blob'path :: !Data.Text.Text,
                       _Blob'language :: !Data.Text.Text,
                       _Blob'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Blob where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Blob "content" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Blob'content (\ x__ y__ -> x__ {_Blob'content = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Blob "path" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Blob'path (\ x__ y__ -> x__ {_Blob'path = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Blob "language" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Blob'language (\ x__ y__ -> x__ {_Blob'language = y__}))
        Prelude.id
instance Data.ProtoLens.Message Blob where
  messageName _ = Data.Text.pack "github.semantic.Blob"
  fieldsByTag
    = let
        content__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "content"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"content")) ::
              Data.ProtoLens.FieldDescriptor Blob
        path__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "path"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"path")) ::
              Data.ProtoLens.FieldDescriptor Blob
        language__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "language"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"language")) ::
              Data.ProtoLens.FieldDescriptor Blob
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, content__field_descriptor),
           (Data.ProtoLens.Tag 2, path__field_descriptor),
           (Data.ProtoLens.Tag 3, language__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Blob'_unknownFields
        (\ x__ y__ -> x__ {_Blob'_unknownFields = y__})
  defMessage
    = Blob'_constructor
        {_Blob'content = Data.ProtoLens.fieldDefault,
         _Blob'path = Data.ProtoLens.fieldDefault,
         _Blob'language = Data.ProtoLens.fieldDefault,
         _Blob'_unknownFields = []}
  parseMessage
    = let
        loop :: Blob -> Data.ProtoLens.Encoding.Bytes.Parser Blob
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "content"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"content") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "path"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "language"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"language") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Blob"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"content") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"language") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Blob where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Blob'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Blob'content x__)
                (Control.DeepSeq.deepseq
                   (_Blob'path x__)
                   (Control.DeepSeq.deepseq (_Blob'language x__) ())))
{- | Fields :
     
         * 'Proto.Semantic_Fields.before' @:: Lens' BlobPair Blob@
         * 'Proto.Semantic_Fields.maybe'before' @:: Lens' BlobPair (Prelude.Maybe Blob)@
         * 'Proto.Semantic_Fields.after' @:: Lens' BlobPair Blob@
         * 'Proto.Semantic_Fields.maybe'after' @:: Lens' BlobPair (Prelude.Maybe Blob)@ -}
data BlobPair
  = BlobPair'_constructor {_BlobPair'before :: !(Prelude.Maybe Blob),
                           _BlobPair'after :: !(Prelude.Maybe Blob),
                           _BlobPair'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show BlobPair where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField BlobPair "before" Blob where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlobPair'before (\ x__ y__ -> x__ {_BlobPair'before = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField BlobPair "maybe'before" (Prelude.Maybe Blob) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlobPair'before (\ x__ y__ -> x__ {_BlobPair'before = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField BlobPair "after" Blob where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlobPair'after (\ x__ y__ -> x__ {_BlobPair'after = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField BlobPair "maybe'after" (Prelude.Maybe Blob) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _BlobPair'after (\ x__ y__ -> x__ {_BlobPair'after = y__}))
        Prelude.id
instance Data.ProtoLens.Message BlobPair where
  messageName _ = Data.Text.pack "github.semantic.BlobPair"
  fieldsByTag
    = let
        before__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "before"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Blob)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'before")) ::
              Data.ProtoLens.FieldDescriptor BlobPair
        after__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "after"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Blob)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'after")) ::
              Data.ProtoLens.FieldDescriptor BlobPair
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, before__field_descriptor),
           (Data.ProtoLens.Tag 2, after__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _BlobPair'_unknownFields
        (\ x__ y__ -> x__ {_BlobPair'_unknownFields = y__})
  defMessage
    = BlobPair'_constructor
        {_BlobPair'before = Prelude.Nothing,
         _BlobPair'after = Prelude.Nothing, _BlobPair'_unknownFields = []}
  parseMessage
    = let
        loop :: BlobPair -> Data.ProtoLens.Encoding.Bytes.Parser BlobPair
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "before"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"before") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "after"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"after") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "BlobPair"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'before") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'after") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData BlobPair where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_BlobPair'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_BlobPair'before x__)
                (Control.DeepSeq.deepseq (_BlobPair'after x__) ()))
newtype ChangeType'UnrecognizedValue
  = ChangeType'UnrecognizedValue Data.Int.Int32
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
data ChangeType
  = NONE |
    ADDED |
    REMOVED |
    MODIFIED |
    ChangeType'Unrecognized !ChangeType'UnrecognizedValue
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
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded ChangeType where
  minBound = NONE
  maxBound = MODIFIED
instance Prelude.Enum ChangeType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum ChangeType: " (Prelude.show k__)))
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
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Semantic_Fields.term' @:: Lens' DeletedTerm Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' DeletedTerm Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' DeletedTerm (Prelude.Maybe Span)@ -}
data DeletedTerm
  = DeletedTerm'_constructor {_DeletedTerm'term :: !Data.Text.Text,
                              _DeletedTerm'span :: !(Prelude.Maybe Span),
                              _DeletedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeletedTerm where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DeletedTerm "term" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeletedTerm'term (\ x__ y__ -> x__ {_DeletedTerm'term = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DeletedTerm "span" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeletedTerm'span (\ x__ y__ -> x__ {_DeletedTerm'span = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField DeletedTerm "maybe'span" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeletedTerm'span (\ x__ y__ -> x__ {_DeletedTerm'span = y__}))
        Prelude.id
instance Data.ProtoLens.Message DeletedTerm where
  messageName _ = Data.Text.pack "github.semantic.DeletedTerm"
  fieldsByTag
    = let
        term__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "term"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"term")) ::
              Data.ProtoLens.FieldDescriptor DeletedTerm
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor DeletedTerm
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, term__field_descriptor),
           (Data.ProtoLens.Tag 2, span__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DeletedTerm'_unknownFields
        (\ x__ y__ -> x__ {_DeletedTerm'_unknownFields = y__})
  defMessage
    = DeletedTerm'_constructor
        {_DeletedTerm'term = Data.ProtoLens.fieldDefault,
         _DeletedTerm'span = Prelude.Nothing,
         _DeletedTerm'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DeletedTerm -> Data.ProtoLens.Encoding.Bytes.Parser DeletedTerm
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "term"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"term") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DeletedTerm"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"term") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'span") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData DeletedTerm where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DeletedTerm'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DeletedTerm'term x__)
                (Control.DeepSeq.deepseq (_DeletedTerm'span x__) ()))
{- | Fields :
     
         * 'Proto.Semantic_Fields.source' @:: Lens' DiffTreeEdge Data.Int.Int32@
         * 'Proto.Semantic_Fields.target' @:: Lens' DiffTreeEdge Data.Int.Int32@ -}
data DiffTreeEdge
  = DiffTreeEdge'_constructor {_DiffTreeEdge'source :: !Data.Int.Int32,
                               _DiffTreeEdge'target :: !Data.Int.Int32,
                               _DiffTreeEdge'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeEdge where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DiffTreeEdge "source" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeEdge'source
           (\ x__ y__ -> x__ {_DiffTreeEdge'source = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DiffTreeEdge "target" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeEdge'target
           (\ x__ y__ -> x__ {_DiffTreeEdge'target = y__}))
        Prelude.id
instance Data.ProtoLens.Message DiffTreeEdge where
  messageName _ = Data.Text.pack "github.semantic.DiffTreeEdge"
  fieldsByTag
    = let
        source__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"source")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeEdge
        target__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "target"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"target")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeEdge
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, source__field_descriptor),
           (Data.ProtoLens.Tag 2, target__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DiffTreeEdge'_unknownFields
        (\ x__ y__ -> x__ {_DiffTreeEdge'_unknownFields = y__})
  defMessage
    = DiffTreeEdge'_constructor
        {_DiffTreeEdge'source = Data.ProtoLens.fieldDefault,
         _DiffTreeEdge'target = Data.ProtoLens.fieldDefault,
         _DiffTreeEdge'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DiffTreeEdge -> Data.ProtoLens.Encoding.Bytes.Parser DiffTreeEdge
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "source"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"source") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "target"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"target") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DiffTreeEdge"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"source") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"target") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData DiffTreeEdge where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DiffTreeEdge'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DiffTreeEdge'source x__)
                (Control.DeepSeq.deepseq (_DiffTreeEdge'target x__) ()))
{- | Fields :
     
         * 'Proto.Semantic_Fields.path' @:: Lens' DiffTreeFileGraph Data.Text.Text@
         * 'Proto.Semantic_Fields.language' @:: Lens' DiffTreeFileGraph Data.Text.Text@
         * 'Proto.Semantic_Fields.vertices' @:: Lens' DiffTreeFileGraph [DiffTreeVertex]@
         * 'Proto.Semantic_Fields.vec'vertices' @:: Lens' DiffTreeFileGraph (Data.Vector.Vector DiffTreeVertex)@
         * 'Proto.Semantic_Fields.edges' @:: Lens' DiffTreeFileGraph [DiffTreeEdge]@
         * 'Proto.Semantic_Fields.vec'edges' @:: Lens' DiffTreeFileGraph (Data.Vector.Vector DiffTreeEdge)@
         * 'Proto.Semantic_Fields.errors' @:: Lens' DiffTreeFileGraph [ParseError]@
         * 'Proto.Semantic_Fields.vec'errors' @:: Lens' DiffTreeFileGraph (Data.Vector.Vector ParseError)@ -}
data DiffTreeFileGraph
  = DiffTreeFileGraph'_constructor {_DiffTreeFileGraph'path :: !Data.Text.Text,
                                    _DiffTreeFileGraph'language :: !Data.Text.Text,
                                    _DiffTreeFileGraph'vertices :: !(Data.Vector.Vector DiffTreeVertex),
                                    _DiffTreeFileGraph'edges :: !(Data.Vector.Vector DiffTreeEdge),
                                    _DiffTreeFileGraph'errors :: !(Data.Vector.Vector ParseError),
                                    _DiffTreeFileGraph'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeFileGraph where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "path" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'path
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'path = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "language" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'language
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'language = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "vertices" [DiffTreeVertex] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'vertices
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'vertices = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "vec'vertices" (Data.Vector.Vector DiffTreeVertex) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'vertices
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'vertices = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "edges" [DiffTreeEdge] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'edges
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'edges = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "vec'edges" (Data.Vector.Vector DiffTreeEdge) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'edges
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'edges = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "errors" [ParseError] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'errors
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'errors = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DiffTreeFileGraph "vec'errors" (Data.Vector.Vector ParseError) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeFileGraph'errors
           (\ x__ y__ -> x__ {_DiffTreeFileGraph'errors = y__}))
        Prelude.id
instance Data.ProtoLens.Message DiffTreeFileGraph where
  messageName _ = Data.Text.pack "github.semantic.DiffTreeFileGraph"
  fieldsByTag
    = let
        path__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "path"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"path")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
        language__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "language"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"language")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
        vertices__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "vertices"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DiffTreeVertex)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"vertices")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
        edges__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "edges"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DiffTreeEdge)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"edges")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
        errors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "errors"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ParseError)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"errors")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeFileGraph
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, path__field_descriptor),
           (Data.ProtoLens.Tag 2, language__field_descriptor),
           (Data.ProtoLens.Tag 3, vertices__field_descriptor),
           (Data.ProtoLens.Tag 4, edges__field_descriptor),
           (Data.ProtoLens.Tag 5, errors__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DiffTreeFileGraph'_unknownFields
        (\ x__ y__ -> x__ {_DiffTreeFileGraph'_unknownFields = y__})
  defMessage
    = DiffTreeFileGraph'_constructor
        {_DiffTreeFileGraph'path = Data.ProtoLens.fieldDefault,
         _DiffTreeFileGraph'language = Data.ProtoLens.fieldDefault,
         _DiffTreeFileGraph'vertices = Data.Vector.Generic.empty,
         _DiffTreeFileGraph'edges = Data.Vector.Generic.empty,
         _DiffTreeFileGraph'errors = Data.Vector.Generic.empty,
         _DiffTreeFileGraph'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DiffTreeFileGraph
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld DiffTreeEdge
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ParseError
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld DiffTreeVertex
                   -> Data.ProtoLens.Encoding.Bytes.Parser DiffTreeFileGraph
        loop x mutable'edges mutable'errors mutable'vertices
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'edges <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'edges)
                      frozen'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'errors)
                      frozen'vertices <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'vertices)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'edges")
                              frozen'edges
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'errors")
                                 frozen'errors
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'vertices")
                                    frozen'vertices
                                    x))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "path"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y x)
                                  mutable'edges
                                  mutable'errors
                                  mutable'vertices
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "language"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"language") y x)
                                  mutable'edges
                                  mutable'errors
                                  mutable'vertices
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "vertices"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'vertices y)
                                loop x mutable'edges mutable'errors v
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "edges"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'edges y)
                                loop x v mutable'errors mutable'vertices
                        42
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "errors"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'errors y)
                                loop x mutable'edges v mutable'vertices
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'edges
                                  mutable'errors
                                  mutable'vertices
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'edges <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              mutable'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              mutable'vertices <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage
                mutable'edges
                mutable'errors
                mutable'vertices)
          "DiffTreeFileGraph"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"language") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage
                                 _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'vertices") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'edges") _x))
                      ((Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                            (\ _v
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                    ((Prelude..)
                                       (\ bs
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                  (Prelude.fromIntegral
                                                     (Data.ByteString.length bs)))
                                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Data.ProtoLens.encodeMessage
                                       _v))
                            (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'errors") _x))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData DiffTreeFileGraph where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DiffTreeFileGraph'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DiffTreeFileGraph'path x__)
                (Control.DeepSeq.deepseq
                   (_DiffTreeFileGraph'language x__)
                   (Control.DeepSeq.deepseq
                      (_DiffTreeFileGraph'vertices x__)
                      (Control.DeepSeq.deepseq
                         (_DiffTreeFileGraph'edges x__)
                         (Control.DeepSeq.deepseq (_DiffTreeFileGraph'errors x__) ())))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.files' @:: Lens' DiffTreeGraphResponse [DiffTreeFileGraph]@
         * 'Proto.Semantic_Fields.vec'files' @:: Lens' DiffTreeGraphResponse (Data.Vector.Vector DiffTreeFileGraph)@ -}
data DiffTreeGraphResponse
  = DiffTreeGraphResponse'_constructor {_DiffTreeGraphResponse'files :: !(Data.Vector.Vector DiffTreeFileGraph),
                                        _DiffTreeGraphResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeGraphResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DiffTreeGraphResponse "files" [DiffTreeFileGraph] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeGraphResponse'files
           (\ x__ y__ -> x__ {_DiffTreeGraphResponse'files = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DiffTreeGraphResponse "vec'files" (Data.Vector.Vector DiffTreeFileGraph) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeGraphResponse'files
           (\ x__ y__ -> x__ {_DiffTreeGraphResponse'files = y__}))
        Prelude.id
instance Data.ProtoLens.Message DiffTreeGraphResponse where
  messageName _
    = Data.Text.pack "github.semantic.DiffTreeGraphResponse"
  fieldsByTag
    = let
        files__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "files"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DiffTreeFileGraph)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"files")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeGraphResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DiffTreeGraphResponse'_unknownFields
        (\ x__ y__ -> x__ {_DiffTreeGraphResponse'_unknownFields = y__})
  defMessage
    = DiffTreeGraphResponse'_constructor
        {_DiffTreeGraphResponse'files = Data.Vector.Generic.empty,
         _DiffTreeGraphResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DiffTreeGraphResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld DiffTreeFileGraph
             -> Data.ProtoLens.Encoding.Bytes.Parser DiffTreeGraphResponse
        loop x mutable'files
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'files)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'files") frozen'files x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "files"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'files y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'files
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'files)
          "DiffTreeGraphResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'files") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData DiffTreeGraphResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DiffTreeGraphResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DiffTreeGraphResponse'files x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.blobs' @:: Lens' DiffTreeRequest [BlobPair]@
         * 'Proto.Semantic_Fields.vec'blobs' @:: Lens' DiffTreeRequest (Data.Vector.Vector BlobPair)@ -}
data DiffTreeRequest
  = DiffTreeRequest'_constructor {_DiffTreeRequest'blobs :: !(Data.Vector.Vector BlobPair),
                                  _DiffTreeRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DiffTreeRequest "blobs" [BlobPair] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeRequest'blobs
           (\ x__ y__ -> x__ {_DiffTreeRequest'blobs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DiffTreeRequest "vec'blobs" (Data.Vector.Vector BlobPair) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeRequest'blobs
           (\ x__ y__ -> x__ {_DiffTreeRequest'blobs = y__}))
        Prelude.id
instance Data.ProtoLens.Message DiffTreeRequest where
  messageName _ = Data.Text.pack "github.semantic.DiffTreeRequest"
  fieldsByTag
    = let
        blobs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "blobs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor BlobPair)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"blobs")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeRequest
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, blobs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DiffTreeRequest'_unknownFields
        (\ x__ y__ -> x__ {_DiffTreeRequest'_unknownFields = y__})
  defMessage
    = DiffTreeRequest'_constructor
        {_DiffTreeRequest'blobs = Data.Vector.Generic.empty,
         _DiffTreeRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DiffTreeRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld BlobPair
             -> Data.ProtoLens.Encoding.Bytes.Parser DiffTreeRequest
        loop x mutable'blobs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'blobs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'blobs)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'blobs") frozen'blobs x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "blobs"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'blobs y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'blobs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'blobs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'blobs)
          "DiffTreeRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'blobs") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData DiffTreeRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DiffTreeRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DiffTreeRequest'blobs x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.files' @:: Lens' DiffTreeTOCResponse [TOCSummaryFile]@
         * 'Proto.Semantic_Fields.vec'files' @:: Lens' DiffTreeTOCResponse (Data.Vector.Vector TOCSummaryFile)@ -}
data DiffTreeTOCResponse
  = DiffTreeTOCResponse'_constructor {_DiffTreeTOCResponse'files :: !(Data.Vector.Vector TOCSummaryFile),
                                      _DiffTreeTOCResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeTOCResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DiffTreeTOCResponse "files" [TOCSummaryFile] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeTOCResponse'files
           (\ x__ y__ -> x__ {_DiffTreeTOCResponse'files = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField DiffTreeTOCResponse "vec'files" (Data.Vector.Vector TOCSummaryFile) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeTOCResponse'files
           (\ x__ y__ -> x__ {_DiffTreeTOCResponse'files = y__}))
        Prelude.id
instance Data.ProtoLens.Message DiffTreeTOCResponse where
  messageName _
    = Data.Text.pack "github.semantic.DiffTreeTOCResponse"
  fieldsByTag
    = let
        files__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "files"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TOCSummaryFile)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"files")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeTOCResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DiffTreeTOCResponse'_unknownFields
        (\ x__ y__ -> x__ {_DiffTreeTOCResponse'_unknownFields = y__})
  defMessage
    = DiffTreeTOCResponse'_constructor
        {_DiffTreeTOCResponse'files = Data.Vector.Generic.empty,
         _DiffTreeTOCResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DiffTreeTOCResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TOCSummaryFile
             -> Data.ProtoLens.Encoding.Bytes.Parser DiffTreeTOCResponse
        loop x mutable'files
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'files)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'files") frozen'files x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "files"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'files y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'files
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'files)
          "DiffTreeTOCResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'files") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData DiffTreeTOCResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DiffTreeTOCResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DiffTreeTOCResponse'files x__) ())
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
         * 'Proto.Semantic_Fields.merged' @:: Lens' DiffTreeVertex MergedTerm@ -}
data DiffTreeVertex
  = DiffTreeVertex'_constructor {_DiffTreeVertex'diffVertexId :: !Data.Int.Int32,
                                 _DiffTreeVertex'diffTerm :: !(Prelude.Maybe DiffTreeVertex'DiffTerm),
                                 _DiffTreeVertex'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DiffTreeVertex where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
data DiffTreeVertex'DiffTerm
  = DiffTreeVertex'Deleted !DeletedTerm |
    DiffTreeVertex'Inserted !InsertedTerm |
    DiffTreeVertex'Replaced !ReplacedTerm |
    DiffTreeVertex'Merged !MergedTerm
  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.Field.HasField DiffTreeVertex "diffVertexId" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffVertexId
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffVertexId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DiffTreeVertex "maybe'diffTerm" (Prelude.Maybe DiffTreeVertex'DiffTerm) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField DiffTreeVertex "maybe'deleted" (Prelude.Maybe DeletedTerm) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DiffTreeVertex'Deleted x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DiffTreeVertex'Deleted y__))
instance Data.ProtoLens.Field.HasField DiffTreeVertex "deleted" DeletedTerm where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DiffTreeVertex'Deleted x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DiffTreeVertex'Deleted y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DiffTreeVertex "maybe'inserted" (Prelude.Maybe InsertedTerm) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DiffTreeVertex'Inserted x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DiffTreeVertex'Inserted y__))
instance Data.ProtoLens.Field.HasField DiffTreeVertex "inserted" InsertedTerm where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DiffTreeVertex'Inserted x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DiffTreeVertex'Inserted y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DiffTreeVertex "maybe'replaced" (Prelude.Maybe ReplacedTerm) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DiffTreeVertex'Replaced x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DiffTreeVertex'Replaced y__))
instance Data.ProtoLens.Field.HasField DiffTreeVertex "replaced" ReplacedTerm where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DiffTreeVertex'Replaced x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DiffTreeVertex'Replaced y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Field.HasField DiffTreeVertex "maybe'merged" (Prelude.Maybe MergedTerm) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        (Lens.Family2.Unchecked.lens
           (\ x__
              -> case x__ of
                   (Prelude.Just (DiffTreeVertex'Merged x__val))
                     -> Prelude.Just x__val
                   _otherwise -> Prelude.Nothing)
           (\ _ y__ -> Prelude.fmap DiffTreeVertex'Merged y__))
instance Data.ProtoLens.Field.HasField DiffTreeVertex "merged" MergedTerm where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DiffTreeVertex'diffTerm
           (\ x__ y__ -> x__ {_DiffTreeVertex'diffTerm = y__}))
        ((Prelude..)
           (Lens.Family2.Unchecked.lens
              (\ x__
                 -> case x__ of
                      (Prelude.Just (DiffTreeVertex'Merged x__val))
                        -> Prelude.Just x__val
                      _otherwise -> Prelude.Nothing)
              (\ _ y__ -> Prelude.fmap DiffTreeVertex'Merged y__))
           (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage))
instance Data.ProtoLens.Message DiffTreeVertex where
  messageName _ = Data.Text.pack "github.semantic.DiffTreeVertex"
  fieldsByTag
    = let
        diffVertexId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "diff_vertex_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"diffVertexId")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeVertex
        deleted__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "deleted"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor DeletedTerm)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'deleted")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeVertex
        inserted__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "inserted"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor InsertedTerm)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'inserted")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeVertex
        replaced__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "replaced"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ReplacedTerm)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'replaced")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeVertex
        merged__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "merged"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor MergedTerm)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'merged")) ::
              Data.ProtoLens.FieldDescriptor DiffTreeVertex
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, diffVertexId__field_descriptor),
           (Data.ProtoLens.Tag 2, deleted__field_descriptor),
           (Data.ProtoLens.Tag 3, inserted__field_descriptor),
           (Data.ProtoLens.Tag 4, replaced__field_descriptor),
           (Data.ProtoLens.Tag 5, merged__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DiffTreeVertex'_unknownFields
        (\ x__ y__ -> x__ {_DiffTreeVertex'_unknownFields = y__})
  defMessage
    = DiffTreeVertex'_constructor
        {_DiffTreeVertex'diffVertexId = Data.ProtoLens.fieldDefault,
         _DiffTreeVertex'diffTerm = Prelude.Nothing,
         _DiffTreeVertex'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DiffTreeVertex
          -> Data.ProtoLens.Encoding.Bytes.Parser DiffTreeVertex
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "diff_vertex_id"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"diffVertexId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "deleted"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"deleted") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "inserted"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"inserted") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "replaced"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"replaced") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "merged"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"merged") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DiffTreeVertex"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"diffVertexId") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'diffTerm") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just (DiffTreeVertex'Deleted v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             v)
                   (Prelude.Just (DiffTreeVertex'Inserted v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             v)
                   (Prelude.Just (DiffTreeVertex'Replaced v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             v)
                   (Prelude.Just (DiffTreeVertex'Merged v))
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData DiffTreeVertex where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DiffTreeVertex'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_DiffTreeVertex'diffVertexId x__)
                (Control.DeepSeq.deepseq (_DiffTreeVertex'diffTerm x__) ()))
instance Control.DeepSeq.NFData DiffTreeVertex'DiffTerm where
  rnf (DiffTreeVertex'Deleted x__) = Control.DeepSeq.rnf x__
  rnf (DiffTreeVertex'Inserted x__) = Control.DeepSeq.rnf x__
  rnf (DiffTreeVertex'Replaced x__) = Control.DeepSeq.rnf x__
  rnf (DiffTreeVertex'Merged x__) = Control.DeepSeq.rnf x__
_DiffTreeVertex'Deleted ::
  Data.ProtoLens.Prism.Prism' DiffTreeVertex'DiffTerm DeletedTerm
_DiffTreeVertex'Deleted
  = Data.ProtoLens.Prism.prism'
      DiffTreeVertex'Deleted
      (\ p__
         -> case p__ of
              (DiffTreeVertex'Deleted p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DiffTreeVertex'Inserted ::
  Data.ProtoLens.Prism.Prism' DiffTreeVertex'DiffTerm InsertedTerm
_DiffTreeVertex'Inserted
  = Data.ProtoLens.Prism.prism'
      DiffTreeVertex'Inserted
      (\ p__
         -> case p__ of
              (DiffTreeVertex'Inserted p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DiffTreeVertex'Replaced ::
  Data.ProtoLens.Prism.Prism' DiffTreeVertex'DiffTerm ReplacedTerm
_DiffTreeVertex'Replaced
  = Data.ProtoLens.Prism.prism'
      DiffTreeVertex'Replaced
      (\ p__
         -> case p__ of
              (DiffTreeVertex'Replaced p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
_DiffTreeVertex'Merged ::
  Data.ProtoLens.Prism.Prism' DiffTreeVertex'DiffTerm MergedTerm
_DiffTreeVertex'Merged
  = Data.ProtoLens.Prism.prism'
      DiffTreeVertex'Merged
      (\ p__
         -> case p__ of
              (DiffTreeVertex'Merged p__val) -> Prelude.Just p__val
              _otherwise -> Prelude.Nothing)
{- | Fields :
     
         * 'Proto.Semantic_Fields.docstring' @:: Lens' Docstring Data.Text.Text@ -}
data Docstring
  = Docstring'_constructor {_Docstring'docstring :: !Data.Text.Text,
                            _Docstring'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Docstring where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Docstring "docstring" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Docstring'docstring
           (\ x__ y__ -> x__ {_Docstring'docstring = y__}))
        Prelude.id
instance Data.ProtoLens.Message Docstring where
  messageName _ = Data.Text.pack "github.semantic.Docstring"
  fieldsByTag
    = let
        docstring__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "docstring"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"docstring")) ::
              Data.ProtoLens.FieldDescriptor Docstring
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, docstring__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Docstring'_unknownFields
        (\ x__ y__ -> x__ {_Docstring'_unknownFields = y__})
  defMessage
    = Docstring'_constructor
        {_Docstring'docstring = Data.ProtoLens.fieldDefault,
         _Docstring'_unknownFields = []}
  parseMessage
    = let
        loop :: Docstring -> Data.ProtoLens.Encoding.Bytes.Parser Docstring
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "docstring"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"docstring") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Docstring"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"docstring") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData Docstring where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Docstring'_unknownFields x__)
             (Control.DeepSeq.deepseq (_Docstring'docstring x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.path' @:: Lens' File Data.Text.Text@
         * 'Proto.Semantic_Fields.language' @:: Lens' File Data.Text.Text@
         * 'Proto.Semantic_Fields.symbols' @:: Lens' File [Symbol]@
         * 'Proto.Semantic_Fields.vec'symbols' @:: Lens' File (Data.Vector.Vector Symbol)@
         * 'Proto.Semantic_Fields.errors' @:: Lens' File [ParseError]@
         * 'Proto.Semantic_Fields.vec'errors' @:: Lens' File (Data.Vector.Vector ParseError)@
         * 'Proto.Semantic_Fields.blobOid' @:: Lens' File Data.Text.Text@ -}
data File
  = File'_constructor {_File'path :: !Data.Text.Text,
                       _File'language :: !Data.Text.Text,
                       _File'symbols :: !(Data.Vector.Vector Symbol),
                       _File'errors :: !(Data.Vector.Vector ParseError),
                       _File'blobOid :: !Data.Text.Text,
                       _File'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show File where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField File "path" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _File'path (\ x__ y__ -> x__ {_File'path = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField File "language" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _File'language (\ x__ y__ -> x__ {_File'language = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField File "symbols" [Symbol] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _File'symbols (\ x__ y__ -> x__ {_File'symbols = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField File "vec'symbols" (Data.Vector.Vector Symbol) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _File'symbols (\ x__ y__ -> x__ {_File'symbols = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField File "errors" [ParseError] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _File'errors (\ x__ y__ -> x__ {_File'errors = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField File "vec'errors" (Data.Vector.Vector ParseError) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _File'errors (\ x__ y__ -> x__ {_File'errors = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField File "blobOid" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _File'blobOid (\ x__ y__ -> x__ {_File'blobOid = y__}))
        Prelude.id
instance Data.ProtoLens.Message File where
  messageName _ = Data.Text.pack "github.semantic.File"
  fieldsByTag
    = let
        path__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "path"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"path")) ::
              Data.ProtoLens.FieldDescriptor File
        language__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "language"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"language")) ::
              Data.ProtoLens.FieldDescriptor File
        symbols__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "symbols"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Symbol)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"symbols")) ::
              Data.ProtoLens.FieldDescriptor File
        errors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "errors"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ParseError)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"errors")) ::
              Data.ProtoLens.FieldDescriptor File
        blobOid__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "blob_oid"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"blobOid")) ::
              Data.ProtoLens.FieldDescriptor File
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, path__field_descriptor),
           (Data.ProtoLens.Tag 2, language__field_descriptor),
           (Data.ProtoLens.Tag 3, symbols__field_descriptor),
           (Data.ProtoLens.Tag 4, errors__field_descriptor),
           (Data.ProtoLens.Tag 5, blobOid__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _File'_unknownFields
        (\ x__ y__ -> x__ {_File'_unknownFields = y__})
  defMessage
    = File'_constructor
        {_File'path = Data.ProtoLens.fieldDefault,
         _File'language = Data.ProtoLens.fieldDefault,
         _File'symbols = Data.Vector.Generic.empty,
         _File'errors = Data.Vector.Generic.empty,
         _File'blobOid = Data.ProtoLens.fieldDefault,
         _File'_unknownFields = []}
  parseMessage
    = let
        loop ::
          File
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ParseError
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Symbol
                -> Data.ProtoLens.Encoding.Bytes.Parser File
        loop x mutable'errors mutable'symbols
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'errors)
                      frozen'symbols <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'symbols)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'errors")
                              frozen'errors
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'symbols") frozen'symbols x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "path"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y x)
                                  mutable'errors
                                  mutable'symbols
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "language"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"language") y x)
                                  mutable'errors
                                  mutable'symbols
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "symbols"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'symbols y)
                                loop x mutable'errors v
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "errors"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'errors y)
                                loop x v mutable'symbols
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "blob_oid"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"blobOid") y x)
                                  mutable'errors
                                  mutable'symbols
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'errors
                                  mutable'symbols
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              mutable'symbols <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'errors mutable'symbols)
          "File"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"language") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage
                                 _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'symbols") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'errors") _x))
                      ((Data.Monoid.<>)
                         (let
                            _v = Lens.Family2.view (Data.ProtoLens.Field.field @"blobOid") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                  ((Prelude..)
                                     (\ bs
                                        -> (Data.Monoid.<>)
                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                (Prelude.fromIntegral (Data.ByteString.length bs)))
                                             (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                     Data.Text.Encoding.encodeUtf8
                                     _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData File where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_File'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_File'path x__)
                (Control.DeepSeq.deepseq
                   (_File'language x__)
                   (Control.DeepSeq.deepseq
                      (_File'symbols x__)
                      (Control.DeepSeq.deepseq
                         (_File'errors x__)
                         (Control.DeepSeq.deepseq (_File'blobOid x__) ())))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.term' @:: Lens' InsertedTerm Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' InsertedTerm Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' InsertedTerm (Prelude.Maybe Span)@ -}
data InsertedTerm
  = InsertedTerm'_constructor {_InsertedTerm'term :: !Data.Text.Text,
                               _InsertedTerm'span :: !(Prelude.Maybe Span),
                               _InsertedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show InsertedTerm where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField InsertedTerm "term" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InsertedTerm'term (\ x__ y__ -> x__ {_InsertedTerm'term = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField InsertedTerm "span" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InsertedTerm'span (\ x__ y__ -> x__ {_InsertedTerm'span = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField InsertedTerm "maybe'span" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _InsertedTerm'span (\ x__ y__ -> x__ {_InsertedTerm'span = y__}))
        Prelude.id
instance Data.ProtoLens.Message InsertedTerm where
  messageName _ = Data.Text.pack "github.semantic.InsertedTerm"
  fieldsByTag
    = let
        term__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "term"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"term")) ::
              Data.ProtoLens.FieldDescriptor InsertedTerm
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor InsertedTerm
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, term__field_descriptor),
           (Data.ProtoLens.Tag 2, span__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _InsertedTerm'_unknownFields
        (\ x__ y__ -> x__ {_InsertedTerm'_unknownFields = y__})
  defMessage
    = InsertedTerm'_constructor
        {_InsertedTerm'term = Data.ProtoLens.fieldDefault,
         _InsertedTerm'span = Prelude.Nothing,
         _InsertedTerm'_unknownFields = []}
  parseMessage
    = let
        loop ::
          InsertedTerm -> Data.ProtoLens.Encoding.Bytes.Parser InsertedTerm
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "term"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"term") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "InsertedTerm"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"term") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'span") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData InsertedTerm where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_InsertedTerm'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_InsertedTerm'term x__)
                (Control.DeepSeq.deepseq (_InsertedTerm'span x__) ()))
{- | Fields :
     
         * 'Proto.Semantic_Fields.term' @:: Lens' MergedTerm Data.Text.Text@
         * 'Proto.Semantic_Fields.beforeSpan' @:: Lens' MergedTerm Span@
         * 'Proto.Semantic_Fields.maybe'beforeSpan' @:: Lens' MergedTerm (Prelude.Maybe Span)@
         * 'Proto.Semantic_Fields.afterSpan' @:: Lens' MergedTerm Span@
         * 'Proto.Semantic_Fields.maybe'afterSpan' @:: Lens' MergedTerm (Prelude.Maybe Span)@ -}
data MergedTerm
  = MergedTerm'_constructor {_MergedTerm'term :: !Data.Text.Text,
                             _MergedTerm'beforeSpan :: !(Prelude.Maybe Span),
                             _MergedTerm'afterSpan :: !(Prelude.Maybe Span),
                             _MergedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show MergedTerm where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField MergedTerm "term" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MergedTerm'term (\ x__ y__ -> x__ {_MergedTerm'term = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MergedTerm "beforeSpan" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MergedTerm'beforeSpan
           (\ x__ y__ -> x__ {_MergedTerm'beforeSpan = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField MergedTerm "maybe'beforeSpan" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MergedTerm'beforeSpan
           (\ x__ y__ -> x__ {_MergedTerm'beforeSpan = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField MergedTerm "afterSpan" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MergedTerm'afterSpan
           (\ x__ y__ -> x__ {_MergedTerm'afterSpan = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField MergedTerm "maybe'afterSpan" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _MergedTerm'afterSpan
           (\ x__ y__ -> x__ {_MergedTerm'afterSpan = y__}))
        Prelude.id
instance Data.ProtoLens.Message MergedTerm where
  messageName _ = Data.Text.pack "github.semantic.MergedTerm"
  fieldsByTag
    = let
        term__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "term"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"term")) ::
              Data.ProtoLens.FieldDescriptor MergedTerm
        beforeSpan__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "before_span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'beforeSpan")) ::
              Data.ProtoLens.FieldDescriptor MergedTerm
        afterSpan__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "after_span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'afterSpan")) ::
              Data.ProtoLens.FieldDescriptor MergedTerm
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, term__field_descriptor),
           (Data.ProtoLens.Tag 2, beforeSpan__field_descriptor),
           (Data.ProtoLens.Tag 3, afterSpan__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _MergedTerm'_unknownFields
        (\ x__ y__ -> x__ {_MergedTerm'_unknownFields = y__})
  defMessage
    = MergedTerm'_constructor
        {_MergedTerm'term = Data.ProtoLens.fieldDefault,
         _MergedTerm'beforeSpan = Prelude.Nothing,
         _MergedTerm'afterSpan = Prelude.Nothing,
         _MergedTerm'_unknownFields = []}
  parseMessage
    = let
        loop ::
          MergedTerm -> Data.ProtoLens.Encoding.Bytes.Parser MergedTerm
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "term"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"term") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "before_span"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"beforeSpan") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "after_span"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"afterSpan") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "MergedTerm"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"term") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'beforeSpan") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view
                          (Data.ProtoLens.Field.field @"maybe'afterSpan") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData MergedTerm where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_MergedTerm'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_MergedTerm'term x__)
                (Control.DeepSeq.deepseq
                   (_MergedTerm'beforeSpan x__)
                   (Control.DeepSeq.deepseq (_MergedTerm'afterSpan x__) ())))
{- | Fields :
     
         * 'Proto.Semantic_Fields.error' @:: Lens' ParseError Data.Text.Text@ -}
data ParseError
  = ParseError'_constructor {_ParseError'error :: !Data.Text.Text,
                             _ParseError'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseError where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ParseError "error" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseError'error (\ x__ y__ -> x__ {_ParseError'error = y__}))
        Prelude.id
instance Data.ProtoLens.Message ParseError where
  messageName _ = Data.Text.pack "github.semantic.ParseError"
  fieldsByTag
    = let
        error__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"error")) ::
              Data.ProtoLens.FieldDescriptor ParseError
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, error__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ParseError'_unknownFields
        (\ x__ y__ -> x__ {_ParseError'_unknownFields = y__})
  defMessage
    = ParseError'_constructor
        {_ParseError'error = Data.ProtoLens.fieldDefault,
         _ParseError'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ParseError -> Data.ProtoLens.Encoding.Bytes.Parser ParseError
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "error"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"error") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ParseError"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"error") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ParseError where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ParseError'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ParseError'error x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.path' @:: Lens' ParseTreeFileGraph Data.Text.Text@
         * 'Proto.Semantic_Fields.language' @:: Lens' ParseTreeFileGraph Data.Text.Text@
         * 'Proto.Semantic_Fields.vertices' @:: Lens' ParseTreeFileGraph [TermVertex]@
         * 'Proto.Semantic_Fields.vec'vertices' @:: Lens' ParseTreeFileGraph (Data.Vector.Vector TermVertex)@
         * 'Proto.Semantic_Fields.edges' @:: Lens' ParseTreeFileGraph [TermEdge]@
         * 'Proto.Semantic_Fields.vec'edges' @:: Lens' ParseTreeFileGraph (Data.Vector.Vector TermEdge)@
         * 'Proto.Semantic_Fields.errors' @:: Lens' ParseTreeFileGraph [ParseError]@
         * 'Proto.Semantic_Fields.vec'errors' @:: Lens' ParseTreeFileGraph (Data.Vector.Vector ParseError)@ -}
data ParseTreeFileGraph
  = ParseTreeFileGraph'_constructor {_ParseTreeFileGraph'path :: !Data.Text.Text,
                                     _ParseTreeFileGraph'language :: !Data.Text.Text,
                                     _ParseTreeFileGraph'vertices :: !(Data.Vector.Vector TermVertex),
                                     _ParseTreeFileGraph'edges :: !(Data.Vector.Vector TermEdge),
                                     _ParseTreeFileGraph'errors :: !(Data.Vector.Vector ParseError),
                                     _ParseTreeFileGraph'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeFileGraph where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "path" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'path
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'path = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "language" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'language
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'language = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "vertices" [TermVertex] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'vertices
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'vertices = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "vec'vertices" (Data.Vector.Vector TermVertex) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'vertices
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'vertices = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "edges" [TermEdge] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'edges
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'edges = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "vec'edges" (Data.Vector.Vector TermEdge) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'edges
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'edges = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "errors" [ParseError] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'errors
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'errors = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ParseTreeFileGraph "vec'errors" (Data.Vector.Vector ParseError) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeFileGraph'errors
           (\ x__ y__ -> x__ {_ParseTreeFileGraph'errors = y__}))
        Prelude.id
instance Data.ProtoLens.Message ParseTreeFileGraph where
  messageName _ = Data.Text.pack "github.semantic.ParseTreeFileGraph"
  fieldsByTag
    = let
        path__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "path"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"path")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
        language__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "language"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"language")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
        vertices__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "vertices"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TermVertex)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"vertices")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
        edges__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "edges"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TermEdge)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"edges")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
        errors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "errors"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ParseError)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"errors")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeFileGraph
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, path__field_descriptor),
           (Data.ProtoLens.Tag 2, language__field_descriptor),
           (Data.ProtoLens.Tag 3, vertices__field_descriptor),
           (Data.ProtoLens.Tag 4, edges__field_descriptor),
           (Data.ProtoLens.Tag 5, errors__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ParseTreeFileGraph'_unknownFields
        (\ x__ y__ -> x__ {_ParseTreeFileGraph'_unknownFields = y__})
  defMessage
    = ParseTreeFileGraph'_constructor
        {_ParseTreeFileGraph'path = Data.ProtoLens.fieldDefault,
         _ParseTreeFileGraph'language = Data.ProtoLens.fieldDefault,
         _ParseTreeFileGraph'vertices = Data.Vector.Generic.empty,
         _ParseTreeFileGraph'edges = Data.Vector.Generic.empty,
         _ParseTreeFileGraph'errors = Data.Vector.Generic.empty,
         _ParseTreeFileGraph'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ParseTreeFileGraph
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TermEdge
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ParseError
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TermVertex
                   -> Data.ProtoLens.Encoding.Bytes.Parser ParseTreeFileGraph
        loop x mutable'edges mutable'errors mutable'vertices
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'edges <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'edges)
                      frozen'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'errors)
                      frozen'vertices <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'vertices)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'edges")
                              frozen'edges
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'errors")
                                 frozen'errors
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'vertices")
                                    frozen'vertices
                                    x))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "path"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y x)
                                  mutable'edges
                                  mutable'errors
                                  mutable'vertices
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "language"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"language") y x)
                                  mutable'edges
                                  mutable'errors
                                  mutable'vertices
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "vertices"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'vertices y)
                                loop x mutable'edges mutable'errors v
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "edges"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'edges y)
                                loop x v mutable'errors mutable'vertices
                        42
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "errors"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'errors y)
                                loop x mutable'edges v mutable'vertices
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'edges
                                  mutable'errors
                                  mutable'vertices
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'edges <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              mutable'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              mutable'vertices <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage
                mutable'edges
                mutable'errors
                mutable'vertices)
          "ParseTreeFileGraph"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"language") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage
                                 _v))
                      (Lens.Family2.view
                         (Data.ProtoLens.Field.field @"vec'vertices") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'edges") _x))
                      ((Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                            (\ _v
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                    ((Prelude..)
                                       (\ bs
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                  (Prelude.fromIntegral
                                                     (Data.ByteString.length bs)))
                                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Data.ProtoLens.encodeMessage
                                       _v))
                            (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'errors") _x))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData ParseTreeFileGraph where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ParseTreeFileGraph'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ParseTreeFileGraph'path x__)
                (Control.DeepSeq.deepseq
                   (_ParseTreeFileGraph'language x__)
                   (Control.DeepSeq.deepseq
                      (_ParseTreeFileGraph'vertices x__)
                      (Control.DeepSeq.deepseq
                         (_ParseTreeFileGraph'edges x__)
                         (Control.DeepSeq.deepseq (_ParseTreeFileGraph'errors x__) ())))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.files' @:: Lens' ParseTreeGraphResponse [ParseTreeFileGraph]@
         * 'Proto.Semantic_Fields.vec'files' @:: Lens' ParseTreeGraphResponse (Data.Vector.Vector ParseTreeFileGraph)@ -}
data ParseTreeGraphResponse
  = ParseTreeGraphResponse'_constructor {_ParseTreeGraphResponse'files :: !(Data.Vector.Vector ParseTreeFileGraph),
                                         _ParseTreeGraphResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeGraphResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ParseTreeGraphResponse "files" [ParseTreeFileGraph] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeGraphResponse'files
           (\ x__ y__ -> x__ {_ParseTreeGraphResponse'files = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ParseTreeGraphResponse "vec'files" (Data.Vector.Vector ParseTreeFileGraph) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeGraphResponse'files
           (\ x__ y__ -> x__ {_ParseTreeGraphResponse'files = y__}))
        Prelude.id
instance Data.ProtoLens.Message ParseTreeGraphResponse where
  messageName _
    = Data.Text.pack "github.semantic.ParseTreeGraphResponse"
  fieldsByTag
    = let
        files__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "files"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ParseTreeFileGraph)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"files")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeGraphResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ParseTreeGraphResponse'_unknownFields
        (\ x__ y__ -> x__ {_ParseTreeGraphResponse'_unknownFields = y__})
  defMessage
    = ParseTreeGraphResponse'_constructor
        {_ParseTreeGraphResponse'files = Data.Vector.Generic.empty,
         _ParseTreeGraphResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ParseTreeGraphResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ParseTreeFileGraph
             -> Data.ProtoLens.Encoding.Bytes.Parser ParseTreeGraphResponse
        loop x mutable'files
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'files)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'files") frozen'files x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "files"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'files y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'files
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'files)
          "ParseTreeGraphResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'files") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ParseTreeGraphResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ParseTreeGraphResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ParseTreeGraphResponse'files x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.blobs' @:: Lens' ParseTreeRequest [Blob]@
         * 'Proto.Semantic_Fields.vec'blobs' @:: Lens' ParseTreeRequest (Data.Vector.Vector Blob)@ -}
data ParseTreeRequest
  = ParseTreeRequest'_constructor {_ParseTreeRequest'blobs :: !(Data.Vector.Vector Blob),
                                   _ParseTreeRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ParseTreeRequest "blobs" [Blob] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeRequest'blobs
           (\ x__ y__ -> x__ {_ParseTreeRequest'blobs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ParseTreeRequest "vec'blobs" (Data.Vector.Vector Blob) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeRequest'blobs
           (\ x__ y__ -> x__ {_ParseTreeRequest'blobs = y__}))
        Prelude.id
instance Data.ProtoLens.Message ParseTreeRequest where
  messageName _ = Data.Text.pack "github.semantic.ParseTreeRequest"
  fieldsByTag
    = let
        blobs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "blobs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Blob)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"blobs")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeRequest
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, blobs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ParseTreeRequest'_unknownFields
        (\ x__ y__ -> x__ {_ParseTreeRequest'_unknownFields = y__})
  defMessage
    = ParseTreeRequest'_constructor
        {_ParseTreeRequest'blobs = Data.Vector.Generic.empty,
         _ParseTreeRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ParseTreeRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Blob
             -> Data.ProtoLens.Encoding.Bytes.Parser ParseTreeRequest
        loop x mutable'blobs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'blobs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'blobs)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'blobs") frozen'blobs x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "blobs"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'blobs y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'blobs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'blobs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'blobs)
          "ParseTreeRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'blobs") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ParseTreeRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ParseTreeRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ParseTreeRequest'blobs x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.files' @:: Lens' ParseTreeSymbolResponse [File]@
         * 'Proto.Semantic_Fields.vec'files' @:: Lens' ParseTreeSymbolResponse (Data.Vector.Vector File)@ -}
data ParseTreeSymbolResponse
  = ParseTreeSymbolResponse'_constructor {_ParseTreeSymbolResponse'files :: !(Data.Vector.Vector File),
                                          _ParseTreeSymbolResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ParseTreeSymbolResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ParseTreeSymbolResponse "files" [File] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeSymbolResponse'files
           (\ x__ y__ -> x__ {_ParseTreeSymbolResponse'files = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ParseTreeSymbolResponse "vec'files" (Data.Vector.Vector File) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ParseTreeSymbolResponse'files
           (\ x__ y__ -> x__ {_ParseTreeSymbolResponse'files = y__}))
        Prelude.id
instance Data.ProtoLens.Message ParseTreeSymbolResponse where
  messageName _
    = Data.Text.pack "github.semantic.ParseTreeSymbolResponse"
  fieldsByTag
    = let
        files__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "files"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor File)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"files")) ::
              Data.ProtoLens.FieldDescriptor ParseTreeSymbolResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ParseTreeSymbolResponse'_unknownFields
        (\ x__ y__ -> x__ {_ParseTreeSymbolResponse'_unknownFields = y__})
  defMessage
    = ParseTreeSymbolResponse'_constructor
        {_ParseTreeSymbolResponse'files = Data.Vector.Generic.empty,
         _ParseTreeSymbolResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ParseTreeSymbolResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld File
             -> Data.ProtoLens.Encoding.Bytes.Parser ParseTreeSymbolResponse
        loop x mutable'files
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'files)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'files") frozen'files x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "files"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'files y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'files
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'files)
          "ParseTreeSymbolResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'files") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ParseTreeSymbolResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ParseTreeSymbolResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ParseTreeSymbolResponse'files x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.service' @:: Lens' PingRequest Data.Text.Text@ -}
data PingRequest
  = PingRequest'_constructor {_PingRequest'service :: !Data.Text.Text,
                              _PingRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PingRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PingRequest "service" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PingRequest'service
           (\ x__ y__ -> x__ {_PingRequest'service = y__}))
        Prelude.id
instance Data.ProtoLens.Message PingRequest where
  messageName _ = Data.Text.pack "github.semantic.PingRequest"
  fieldsByTag
    = let
        service__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "service"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"service")) ::
              Data.ProtoLens.FieldDescriptor PingRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, service__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PingRequest'_unknownFields
        (\ x__ y__ -> x__ {_PingRequest'_unknownFields = y__})
  defMessage
    = PingRequest'_constructor
        {_PingRequest'service = Data.ProtoLens.fieldDefault,
         _PingRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PingRequest -> Data.ProtoLens.Encoding.Bytes.Parser PingRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "service"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"service") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PingRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"service") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData PingRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PingRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_PingRequest'service x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.status' @:: Lens' PingResponse Data.Text.Text@
         * 'Proto.Semantic_Fields.hostname' @:: Lens' PingResponse Data.Text.Text@
         * 'Proto.Semantic_Fields.timestamp' @:: Lens' PingResponse Data.Text.Text@
         * 'Proto.Semantic_Fields.sha' @:: Lens' PingResponse Data.Text.Text@ -}
data PingResponse
  = PingResponse'_constructor {_PingResponse'status :: !Data.Text.Text,
                               _PingResponse'hostname :: !Data.Text.Text,
                               _PingResponse'timestamp :: !Data.Text.Text,
                               _PingResponse'sha :: !Data.Text.Text,
                               _PingResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show PingResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField PingResponse "status" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PingResponse'status
           (\ x__ y__ -> x__ {_PingResponse'status = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PingResponse "hostname" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PingResponse'hostname
           (\ x__ y__ -> x__ {_PingResponse'hostname = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PingResponse "timestamp" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PingResponse'timestamp
           (\ x__ y__ -> x__ {_PingResponse'timestamp = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField PingResponse "sha" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _PingResponse'sha (\ x__ y__ -> x__ {_PingResponse'sha = y__}))
        Prelude.id
instance Data.ProtoLens.Message PingResponse where
  messageName _ = Data.Text.pack "github.semantic.PingResponse"
  fieldsByTag
    = let
        status__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "status"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"status")) ::
              Data.ProtoLens.FieldDescriptor PingResponse
        hostname__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "hostname"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"hostname")) ::
              Data.ProtoLens.FieldDescriptor PingResponse
        timestamp__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "timestamp"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timestamp")) ::
              Data.ProtoLens.FieldDescriptor PingResponse
        sha__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "sha"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"sha")) ::
              Data.ProtoLens.FieldDescriptor PingResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, status__field_descriptor),
           (Data.ProtoLens.Tag 2, hostname__field_descriptor),
           (Data.ProtoLens.Tag 3, timestamp__field_descriptor),
           (Data.ProtoLens.Tag 4, sha__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _PingResponse'_unknownFields
        (\ x__ y__ -> x__ {_PingResponse'_unknownFields = y__})
  defMessage
    = PingResponse'_constructor
        {_PingResponse'status = Data.ProtoLens.fieldDefault,
         _PingResponse'hostname = Data.ProtoLens.fieldDefault,
         _PingResponse'timestamp = Data.ProtoLens.fieldDefault,
         _PingResponse'sha = Data.ProtoLens.fieldDefault,
         _PingResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          PingResponse -> Data.ProtoLens.Encoding.Bytes.Parser PingResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "status"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"status") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "hostname"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"hostname") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "timestamp"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"timestamp") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "sha"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"sha") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "PingResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"status") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"hostname") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"timestamp") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   ((Data.Monoid.<>)
                      (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"sha") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8
                                  _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData PingResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_PingResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_PingResponse'status x__)
                (Control.DeepSeq.deepseq
                   (_PingResponse'hostname x__)
                   (Control.DeepSeq.deepseq
                      (_PingResponse'timestamp x__)
                      (Control.DeepSeq.deepseq (_PingResponse'sha x__) ()))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.line' @:: Lens' Position Data.Int.Int32@
         * 'Proto.Semantic_Fields.column' @:: Lens' Position Data.Int.Int32@ -}
data Position
  = Position'_constructor {_Position'line :: !Data.Int.Int32,
                           _Position'column :: !Data.Int.Int32,
                           _Position'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Position where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Position "line" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Position'line (\ x__ y__ -> x__ {_Position'line = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Position "column" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Position'column (\ x__ y__ -> x__ {_Position'column = y__}))
        Prelude.id
instance Data.ProtoLens.Message Position where
  messageName _ = Data.Text.pack "github.semantic.Position"
  fieldsByTag
    = let
        line__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "line"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"line")) ::
              Data.ProtoLens.FieldDescriptor Position
        column__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "column"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"column")) ::
              Data.ProtoLens.FieldDescriptor Position
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, line__field_descriptor),
           (Data.ProtoLens.Tag 2, column__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Position'_unknownFields
        (\ x__ y__ -> x__ {_Position'_unknownFields = y__})
  defMessage
    = Position'_constructor
        {_Position'line = Data.ProtoLens.fieldDefault,
         _Position'column = Data.ProtoLens.fieldDefault,
         _Position'_unknownFields = []}
  parseMessage
    = let
        loop :: Position -> Data.ProtoLens.Encoding.Bytes.Parser Position
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "line"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"line") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "column"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"column") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Position"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"line") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"column") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Position where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Position'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Position'line x__)
                (Control.DeepSeq.deepseq (_Position'column x__) ()))
{- | Fields :
     
         * 'Proto.Semantic_Fields.beforeTerm' @:: Lens' ReplacedTerm Data.Text.Text@
         * 'Proto.Semantic_Fields.beforeSpan' @:: Lens' ReplacedTerm Span@
         * 'Proto.Semantic_Fields.maybe'beforeSpan' @:: Lens' ReplacedTerm (Prelude.Maybe Span)@
         * 'Proto.Semantic_Fields.afterTerm' @:: Lens' ReplacedTerm Data.Text.Text@
         * 'Proto.Semantic_Fields.afterSpan' @:: Lens' ReplacedTerm Span@
         * 'Proto.Semantic_Fields.maybe'afterSpan' @:: Lens' ReplacedTerm (Prelude.Maybe Span)@ -}
data ReplacedTerm
  = ReplacedTerm'_constructor {_ReplacedTerm'beforeTerm :: !Data.Text.Text,
                               _ReplacedTerm'beforeSpan :: !(Prelude.Maybe Span),
                               _ReplacedTerm'afterTerm :: !Data.Text.Text,
                               _ReplacedTerm'afterSpan :: !(Prelude.Maybe Span),
                               _ReplacedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ReplacedTerm where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ReplacedTerm "beforeTerm" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReplacedTerm'beforeTerm
           (\ x__ y__ -> x__ {_ReplacedTerm'beforeTerm = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReplacedTerm "beforeSpan" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReplacedTerm'beforeSpan
           (\ x__ y__ -> x__ {_ReplacedTerm'beforeSpan = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReplacedTerm "maybe'beforeSpan" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReplacedTerm'beforeSpan
           (\ x__ y__ -> x__ {_ReplacedTerm'beforeSpan = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReplacedTerm "afterTerm" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReplacedTerm'afterTerm
           (\ x__ y__ -> x__ {_ReplacedTerm'afterTerm = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ReplacedTerm "afterSpan" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReplacedTerm'afterSpan
           (\ x__ y__ -> x__ {_ReplacedTerm'afterSpan = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ReplacedTerm "maybe'afterSpan" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ReplacedTerm'afterSpan
           (\ x__ y__ -> x__ {_ReplacedTerm'afterSpan = y__}))
        Prelude.id
instance Data.ProtoLens.Message ReplacedTerm where
  messageName _ = Data.Text.pack "github.semantic.ReplacedTerm"
  fieldsByTag
    = let
        beforeTerm__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "before_term"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"beforeTerm")) ::
              Data.ProtoLens.FieldDescriptor ReplacedTerm
        beforeSpan__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "before_span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'beforeSpan")) ::
              Data.ProtoLens.FieldDescriptor ReplacedTerm
        afterTerm__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "after_term"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"afterTerm")) ::
              Data.ProtoLens.FieldDescriptor ReplacedTerm
        afterSpan__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "after_span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'afterSpan")) ::
              Data.ProtoLens.FieldDescriptor ReplacedTerm
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, beforeTerm__field_descriptor),
           (Data.ProtoLens.Tag 2, beforeSpan__field_descriptor),
           (Data.ProtoLens.Tag 3, afterTerm__field_descriptor),
           (Data.ProtoLens.Tag 4, afterSpan__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ReplacedTerm'_unknownFields
        (\ x__ y__ -> x__ {_ReplacedTerm'_unknownFields = y__})
  defMessage
    = ReplacedTerm'_constructor
        {_ReplacedTerm'beforeTerm = Data.ProtoLens.fieldDefault,
         _ReplacedTerm'beforeSpan = Prelude.Nothing,
         _ReplacedTerm'afterTerm = Data.ProtoLens.fieldDefault,
         _ReplacedTerm'afterSpan = Prelude.Nothing,
         _ReplacedTerm'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ReplacedTerm -> Data.ProtoLens.Encoding.Bytes.Parser ReplacedTerm
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "before_term"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"beforeTerm") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "before_span"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"beforeSpan") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "after_term"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"afterTerm") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "after_span"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"afterSpan") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ReplacedTerm"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"beforeTerm") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view
                       (Data.ProtoLens.Field.field @"maybe'beforeSpan") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"afterTerm") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view
                             (Data.ProtoLens.Field.field @"maybe'afterSpan") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage
                                   _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData ReplacedTerm where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ReplacedTerm'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ReplacedTerm'beforeTerm x__)
                (Control.DeepSeq.deepseq
                   (_ReplacedTerm'beforeSpan x__)
                   (Control.DeepSeq.deepseq
                      (_ReplacedTerm'afterTerm x__)
                      (Control.DeepSeq.deepseq (_ReplacedTerm'afterSpan x__) ()))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.start' @:: Lens' Span Position@
         * 'Proto.Semantic_Fields.maybe'start' @:: Lens' Span (Prelude.Maybe Position)@
         * 'Proto.Semantic_Fields.end' @:: Lens' Span Position@
         * 'Proto.Semantic_Fields.maybe'end' @:: Lens' Span (Prelude.Maybe Position)@ -}
data Span
  = Span'_constructor {_Span'start :: !(Prelude.Maybe Position),
                       _Span'end :: !(Prelude.Maybe Position),
                       _Span'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Span where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Span "start" Position where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Span'start (\ x__ y__ -> x__ {_Span'start = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Span "maybe'start" (Prelude.Maybe Position) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Span'start (\ x__ y__ -> x__ {_Span'start = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Span "end" Position where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Span'end (\ x__ y__ -> x__ {_Span'end = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Span "maybe'end" (Prelude.Maybe Position) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Span'end (\ x__ y__ -> x__ {_Span'end = y__}))
        Prelude.id
instance Data.ProtoLens.Message Span where
  messageName _ = Data.Text.pack "github.semantic.Span"
  fieldsByTag
    = let
        start__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "start"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Position)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'start")) ::
              Data.ProtoLens.FieldDescriptor Span
        end__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "end"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Position)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'end")) ::
              Data.ProtoLens.FieldDescriptor Span
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, start__field_descriptor),
           (Data.ProtoLens.Tag 2, end__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Span'_unknownFields
        (\ x__ y__ -> x__ {_Span'_unknownFields = y__})
  defMessage
    = Span'_constructor
        {_Span'start = Prelude.Nothing, _Span'end = Prelude.Nothing,
         _Span'_unknownFields = []}
  parseMessage
    = let
        loop :: Span -> Data.ProtoLens.Encoding.Bytes.Parser Span
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "start"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"start") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "end"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"end") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Span"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'start") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage
                          _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'end") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Span where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Span'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Span'start x__) (Control.DeepSeq.deepseq (_Span'end x__) ()))
{- | Fields :
     
         * 'Proto.Semantic_Fields.path' @:: Lens' StackGraphFile Data.Text.Text@
         * 'Proto.Semantic_Fields.language' @:: Lens' StackGraphFile Data.Text.Text@
         * 'Proto.Semantic_Fields.nodes' @:: Lens' StackGraphFile [StackGraphNode]@
         * 'Proto.Semantic_Fields.vec'nodes' @:: Lens' StackGraphFile (Data.Vector.Vector StackGraphNode)@
         * 'Proto.Semantic_Fields.paths' @:: Lens' StackGraphFile [StackGraphPath]@
         * 'Proto.Semantic_Fields.vec'paths' @:: Lens' StackGraphFile (Data.Vector.Vector StackGraphPath)@
         * 'Proto.Semantic_Fields.errors' @:: Lens' StackGraphFile [ParseError]@
         * 'Proto.Semantic_Fields.vec'errors' @:: Lens' StackGraphFile (Data.Vector.Vector ParseError)@ -}
data StackGraphFile
  = StackGraphFile'_constructor {_StackGraphFile'path :: !Data.Text.Text,
                                 _StackGraphFile'language :: !Data.Text.Text,
                                 _StackGraphFile'nodes :: !(Data.Vector.Vector StackGraphNode),
                                 _StackGraphFile'paths :: !(Data.Vector.Vector StackGraphPath),
                                 _StackGraphFile'errors :: !(Data.Vector.Vector ParseError),
                                 _StackGraphFile'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StackGraphFile where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StackGraphFile "path" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'path
           (\ x__ y__ -> x__ {_StackGraphFile'path = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphFile "language" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'language
           (\ x__ y__ -> x__ {_StackGraphFile'language = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphFile "nodes" [StackGraphNode] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'nodes
           (\ x__ y__ -> x__ {_StackGraphFile'nodes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphFile "vec'nodes" (Data.Vector.Vector StackGraphNode) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'nodes
           (\ x__ y__ -> x__ {_StackGraphFile'nodes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphFile "paths" [StackGraphPath] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'paths
           (\ x__ y__ -> x__ {_StackGraphFile'paths = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphFile "vec'paths" (Data.Vector.Vector StackGraphPath) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'paths
           (\ x__ y__ -> x__ {_StackGraphFile'paths = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphFile "errors" [ParseError] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'errors
           (\ x__ y__ -> x__ {_StackGraphFile'errors = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphFile "vec'errors" (Data.Vector.Vector ParseError) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphFile'errors
           (\ x__ y__ -> x__ {_StackGraphFile'errors = y__}))
        Prelude.id
instance Data.ProtoLens.Message StackGraphFile where
  messageName _ = Data.Text.pack "github.semantic.StackGraphFile"
  fieldsByTag
    = let
        path__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "path"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"path")) ::
              Data.ProtoLens.FieldDescriptor StackGraphFile
        language__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "language"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"language")) ::
              Data.ProtoLens.FieldDescriptor StackGraphFile
        nodes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "nodes"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StackGraphNode)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"nodes")) ::
              Data.ProtoLens.FieldDescriptor StackGraphFile
        paths__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "paths"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StackGraphPath)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"paths")) ::
              Data.ProtoLens.FieldDescriptor StackGraphFile
        errors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "errors"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ParseError)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"errors")) ::
              Data.ProtoLens.FieldDescriptor StackGraphFile
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, path__field_descriptor),
           (Data.ProtoLens.Tag 2, language__field_descriptor),
           (Data.ProtoLens.Tag 3, nodes__field_descriptor),
           (Data.ProtoLens.Tag 4, paths__field_descriptor),
           (Data.ProtoLens.Tag 5, errors__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StackGraphFile'_unknownFields
        (\ x__ y__ -> x__ {_StackGraphFile'_unknownFields = y__})
  defMessage
    = StackGraphFile'_constructor
        {_StackGraphFile'path = Data.ProtoLens.fieldDefault,
         _StackGraphFile'language = Data.ProtoLens.fieldDefault,
         _StackGraphFile'nodes = Data.Vector.Generic.empty,
         _StackGraphFile'paths = Data.Vector.Generic.empty,
         _StackGraphFile'errors = Data.Vector.Generic.empty,
         _StackGraphFile'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StackGraphFile
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ParseError
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld StackGraphNode
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld StackGraphPath
                   -> Data.ProtoLens.Encoding.Bytes.Parser StackGraphFile
        loop x mutable'errors mutable'nodes mutable'paths
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'errors)
                      frozen'nodes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'nodes)
                      frozen'paths <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'paths)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'errors")
                              frozen'errors
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'nodes")
                                 frozen'nodes
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'paths") frozen'paths x))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "path"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y x)
                                  mutable'errors
                                  mutable'nodes
                                  mutable'paths
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "language"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"language") y x)
                                  mutable'errors
                                  mutable'nodes
                                  mutable'paths
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "nodes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'nodes y)
                                loop x mutable'errors v mutable'paths
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "paths"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'paths y)
                                loop x mutable'errors mutable'nodes v
                        42
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "errors"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'errors y)
                                loop x v mutable'nodes mutable'paths
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'errors
                                  mutable'nodes
                                  mutable'paths
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              mutable'nodes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              mutable'paths <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage
                mutable'errors
                mutable'nodes
                mutable'paths)
          "StackGraphFile"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"language") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage
                                 _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'nodes") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'paths") _x))
                      ((Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                            (\ _v
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                    ((Prelude..)
                                       (\ bs
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                  (Prelude.fromIntegral
                                                     (Data.ByteString.length bs)))
                                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                       Data.ProtoLens.encodeMessage
                                       _v))
                            (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'errors") _x))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData StackGraphFile where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StackGraphFile'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StackGraphFile'path x__)
                (Control.DeepSeq.deepseq
                   (_StackGraphFile'language x__)
                   (Control.DeepSeq.deepseq
                      (_StackGraphFile'nodes x__)
                      (Control.DeepSeq.deepseq
                         (_StackGraphFile'paths x__)
                         (Control.DeepSeq.deepseq (_StackGraphFile'errors x__) ())))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.id' @:: Lens' StackGraphNode Data.Int.Int64@
         * 'Proto.Semantic_Fields.name' @:: Lens' StackGraphNode Data.Text.Text@
         * 'Proto.Semantic_Fields.line' @:: Lens' StackGraphNode Data.Text.Text@
         * 'Proto.Semantic_Fields.kind' @:: Lens' StackGraphNode Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' StackGraphNode Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' StackGraphNode (Prelude.Maybe Span)@
         * 'Proto.Semantic_Fields.nodeType' @:: Lens' StackGraphNode StackGraphNode'NodeType@ -}
data StackGraphNode
  = StackGraphNode'_constructor {_StackGraphNode'id :: !Data.Int.Int64,
                                 _StackGraphNode'name :: !Data.Text.Text,
                                 _StackGraphNode'line :: !Data.Text.Text,
                                 _StackGraphNode'kind :: !Data.Text.Text,
                                 _StackGraphNode'span :: !(Prelude.Maybe Span),
                                 _StackGraphNode'nodeType :: !StackGraphNode'NodeType,
                                 _StackGraphNode'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StackGraphNode where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StackGraphNode "id" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'id (\ x__ y__ -> x__ {_StackGraphNode'id = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphNode "name" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'name
           (\ x__ y__ -> x__ {_StackGraphNode'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphNode "line" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'line
           (\ x__ y__ -> x__ {_StackGraphNode'line = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphNode "kind" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'kind
           (\ x__ y__ -> x__ {_StackGraphNode'kind = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphNode "span" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'span
           (\ x__ y__ -> x__ {_StackGraphNode'span = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField StackGraphNode "maybe'span" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'span
           (\ x__ y__ -> x__ {_StackGraphNode'span = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphNode "nodeType" StackGraphNode'NodeType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'nodeType
           (\ x__ y__ -> x__ {_StackGraphNode'nodeType = y__}))
        Prelude.id
instance Data.ProtoLens.Message StackGraphNode where
  messageName _ = Data.Text.pack "github.semantic.StackGraphNode"
  fieldsByTag
    = let
        id__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"id")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
        line__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "line"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"line")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
        kind__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "kind"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"kind")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
        nodeType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "node_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor StackGraphNode'NodeType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nodeType")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, id__field_descriptor),
           (Data.ProtoLens.Tag 2, name__field_descriptor),
           (Data.ProtoLens.Tag 3, line__field_descriptor),
           (Data.ProtoLens.Tag 4, kind__field_descriptor),
           (Data.ProtoLens.Tag 5, span__field_descriptor),
           (Data.ProtoLens.Tag 6, nodeType__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StackGraphNode'_unknownFields
        (\ x__ y__ -> x__ {_StackGraphNode'_unknownFields = y__})
  defMessage
    = StackGraphNode'_constructor
        {_StackGraphNode'id = Data.ProtoLens.fieldDefault,
         _StackGraphNode'name = Data.ProtoLens.fieldDefault,
         _StackGraphNode'line = Data.ProtoLens.fieldDefault,
         _StackGraphNode'kind = Data.ProtoLens.fieldDefault,
         _StackGraphNode'span = Prelude.Nothing,
         _StackGraphNode'nodeType = Data.ProtoLens.fieldDefault,
         _StackGraphNode'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StackGraphNode
          -> Data.ProtoLens.Encoding.Bytes.Parser StackGraphNode
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"id") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "line"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"line") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "kind"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"kind") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "node_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"nodeType") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "StackGraphNode"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"id") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"line") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   ((Data.Monoid.<>)
                      (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"kind") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8
                                  _v))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'span") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage
                                      _v))
                         ((Data.Monoid.<>)
                            (let
                               _v = Lens.Family2.view (Data.ProtoLens.Field.field @"nodeType") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                     ((Prelude..)
                                        ((Prelude..)
                                           Data.ProtoLens.Encoding.Bytes.putVarInt
                                           Prelude.fromIntegral)
                                        Prelude.fromEnum
                                        _v))
                            (Data.ProtoLens.Encoding.Wire.buildFieldSet
                               (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))))
instance Control.DeepSeq.NFData StackGraphNode where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StackGraphNode'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StackGraphNode'id x__)
                (Control.DeepSeq.deepseq
                   (_StackGraphNode'name x__)
                   (Control.DeepSeq.deepseq
                      (_StackGraphNode'line x__)
                      (Control.DeepSeq.deepseq
                         (_StackGraphNode'kind x__)
                         (Control.DeepSeq.deepseq
                            (_StackGraphNode'span x__)
                            (Control.DeepSeq.deepseq (_StackGraphNode'nodeType x__) ()))))))
newtype StackGraphNode'NodeType'UnrecognizedValue
  = StackGraphNode'NodeType'UnrecognizedValue Data.Int.Int32
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
data StackGraphNode'NodeType
  = StackGraphNode'ROOT_SCOPE |
    StackGraphNode'JUMP_TO_SCOPE |
    StackGraphNode'EXPORTED_SCOPE |
    StackGraphNode'DEFINITION |
    StackGraphNode'REFERENCE |
    StackGraphNode'NodeType'Unrecognized !StackGraphNode'NodeType'UnrecognizedValue
  deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum StackGraphNode'NodeType where
  maybeToEnum 0 = Prelude.Just StackGraphNode'ROOT_SCOPE
  maybeToEnum 1 = Prelude.Just StackGraphNode'JUMP_TO_SCOPE
  maybeToEnum 2 = Prelude.Just StackGraphNode'EXPORTED_SCOPE
  maybeToEnum 3 = Prelude.Just StackGraphNode'DEFINITION
  maybeToEnum 4 = Prelude.Just StackGraphNode'REFERENCE
  maybeToEnum k
    = Prelude.Just
        (StackGraphNode'NodeType'Unrecognized
           (StackGraphNode'NodeType'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum StackGraphNode'ROOT_SCOPE = "ROOT_SCOPE"
  showEnum StackGraphNode'JUMP_TO_SCOPE = "JUMP_TO_SCOPE"
  showEnum StackGraphNode'EXPORTED_SCOPE = "EXPORTED_SCOPE"
  showEnum StackGraphNode'DEFINITION = "DEFINITION"
  showEnum StackGraphNode'REFERENCE = "REFERENCE"
  showEnum
    (StackGraphNode'NodeType'Unrecognized (StackGraphNode'NodeType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "ROOT_SCOPE"
    = Prelude.Just StackGraphNode'ROOT_SCOPE
    | (Prelude.==) k "JUMP_TO_SCOPE"
    = Prelude.Just StackGraphNode'JUMP_TO_SCOPE
    | (Prelude.==) k "EXPORTED_SCOPE"
    = Prelude.Just StackGraphNode'EXPORTED_SCOPE
    | (Prelude.==) k "DEFINITION"
    = Prelude.Just StackGraphNode'DEFINITION
    | (Prelude.==) k "REFERENCE"
    = Prelude.Just StackGraphNode'REFERENCE
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded StackGraphNode'NodeType where
  minBound = StackGraphNode'ROOT_SCOPE
  maxBound = StackGraphNode'REFERENCE
instance Prelude.Enum StackGraphNode'NodeType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum NodeType: " (Prelude.show k__)))
        Prelude.id
        (Data.ProtoLens.maybeToEnum k__)
  fromEnum StackGraphNode'ROOT_SCOPE = 0
  fromEnum StackGraphNode'JUMP_TO_SCOPE = 1
  fromEnum StackGraphNode'EXPORTED_SCOPE = 2
  fromEnum StackGraphNode'DEFINITION = 3
  fromEnum StackGraphNode'REFERENCE = 4
  fromEnum
    (StackGraphNode'NodeType'Unrecognized (StackGraphNode'NodeType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ StackGraphNode'REFERENCE
    = Prelude.error
        "StackGraphNode'NodeType.succ: bad argument StackGraphNode'REFERENCE. This value would be out of bounds."
  succ StackGraphNode'ROOT_SCOPE = StackGraphNode'JUMP_TO_SCOPE
  succ StackGraphNode'JUMP_TO_SCOPE = StackGraphNode'EXPORTED_SCOPE
  succ StackGraphNode'EXPORTED_SCOPE = StackGraphNode'DEFINITION
  succ StackGraphNode'DEFINITION = StackGraphNode'REFERENCE
  succ (StackGraphNode'NodeType'Unrecognized _)
    = Prelude.error
        "StackGraphNode'NodeType.succ: bad argument: unrecognized value"
  pred StackGraphNode'ROOT_SCOPE
    = Prelude.error
        "StackGraphNode'NodeType.pred: bad argument StackGraphNode'ROOT_SCOPE. This value would be out of bounds."
  pred StackGraphNode'JUMP_TO_SCOPE = StackGraphNode'ROOT_SCOPE
  pred StackGraphNode'EXPORTED_SCOPE = StackGraphNode'JUMP_TO_SCOPE
  pred StackGraphNode'DEFINITION = StackGraphNode'EXPORTED_SCOPE
  pred StackGraphNode'REFERENCE = StackGraphNode'DEFINITION
  pred (StackGraphNode'NodeType'Unrecognized _)
    = Prelude.error
        "StackGraphNode'NodeType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault StackGraphNode'NodeType where
  fieldDefault = StackGraphNode'ROOT_SCOPE
instance Control.DeepSeq.NFData StackGraphNode'NodeType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Semantic_Fields.startingSymbolStack' @:: Lens' StackGraphPath [Data.Text.Text]@
         * 'Proto.Semantic_Fields.vec'startingSymbolStack' @:: Lens' StackGraphPath (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Semantic_Fields.startingScopeStackSize' @:: Lens' StackGraphPath Data.Int.Int64@
         * 'Proto.Semantic_Fields.from' @:: Lens' StackGraphPath Data.Int.Int64@
         * 'Proto.Semantic_Fields.edges' @:: Lens' StackGraphPath Data.Text.Text@
         * 'Proto.Semantic_Fields.to' @:: Lens' StackGraphPath Data.Int.Int64@
         * 'Proto.Semantic_Fields.endingScopeStack' @:: Lens' StackGraphPath [Data.Int.Int64]@
         * 'Proto.Semantic_Fields.vec'endingScopeStack' @:: Lens' StackGraphPath (Data.Vector.Unboxed.Vector Data.Int.Int64)@
         * 'Proto.Semantic_Fields.endingSymbolStack' @:: Lens' StackGraphPath [Data.Text.Text]@
         * 'Proto.Semantic_Fields.vec'endingSymbolStack' @:: Lens' StackGraphPath (Data.Vector.Vector Data.Text.Text)@ -}
data StackGraphPath
  = StackGraphPath'_constructor {_StackGraphPath'startingSymbolStack :: !(Data.Vector.Vector Data.Text.Text),
                                 _StackGraphPath'startingScopeStackSize :: !Data.Int.Int64,
                                 _StackGraphPath'from :: !Data.Int.Int64,
                                 _StackGraphPath'edges :: !Data.Text.Text,
                                 _StackGraphPath'to :: !Data.Int.Int64,
                                 _StackGraphPath'endingScopeStack :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                                 _StackGraphPath'endingSymbolStack :: !(Data.Vector.Vector Data.Text.Text),
                                 _StackGraphPath'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StackGraphPath where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StackGraphPath "startingSymbolStack" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'startingSymbolStack
           (\ x__ y__ -> x__ {_StackGraphPath'startingSymbolStack = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphPath "vec'startingSymbolStack" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'startingSymbolStack
           (\ x__ y__ -> x__ {_StackGraphPath'startingSymbolStack = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphPath "startingScopeStackSize" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'startingScopeStackSize
           (\ x__ y__ -> x__ {_StackGraphPath'startingScopeStackSize = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphPath "from" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'from
           (\ x__ y__ -> x__ {_StackGraphPath'from = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphPath "edges" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'edges
           (\ x__ y__ -> x__ {_StackGraphPath'edges = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphPath "to" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'to (\ x__ y__ -> x__ {_StackGraphPath'to = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphPath "endingScopeStack" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'endingScopeStack
           (\ x__ y__ -> x__ {_StackGraphPath'endingScopeStack = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphPath "vec'endingScopeStack" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'endingScopeStack
           (\ x__ y__ -> x__ {_StackGraphPath'endingScopeStack = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphPath "endingSymbolStack" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'endingSymbolStack
           (\ x__ y__ -> x__ {_StackGraphPath'endingSymbolStack = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphPath "vec'endingSymbolStack" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphPath'endingSymbolStack
           (\ x__ y__ -> x__ {_StackGraphPath'endingSymbolStack = y__}))
        Prelude.id
instance Data.ProtoLens.Message StackGraphPath where
  messageName _ = Data.Text.pack "github.semantic.StackGraphPath"
  fieldsByTag
    = let
        startingSymbolStack__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "starting_symbol_stack"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"startingSymbolStack")) ::
              Data.ProtoLens.FieldDescriptor StackGraphPath
        startingScopeStackSize__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "starting_scope_stack_size"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"startingScopeStackSize")) ::
              Data.ProtoLens.FieldDescriptor StackGraphPath
        from__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "from"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"from")) ::
              Data.ProtoLens.FieldDescriptor StackGraphPath
        edges__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "edges"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"edges")) ::
              Data.ProtoLens.FieldDescriptor StackGraphPath
        to__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "to"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"to")) ::
              Data.ProtoLens.FieldDescriptor StackGraphPath
        endingScopeStack__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ending_scope_stack"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"endingScopeStack")) ::
              Data.ProtoLens.FieldDescriptor StackGraphPath
        endingSymbolStack__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "ending_symbol_stack"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"endingSymbolStack")) ::
              Data.ProtoLens.FieldDescriptor StackGraphPath
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, startingSymbolStack__field_descriptor),
           (Data.ProtoLens.Tag 2, startingScopeStackSize__field_descriptor),
           (Data.ProtoLens.Tag 3, from__field_descriptor),
           (Data.ProtoLens.Tag 4, edges__field_descriptor),
           (Data.ProtoLens.Tag 5, to__field_descriptor),
           (Data.ProtoLens.Tag 6, endingScopeStack__field_descriptor),
           (Data.ProtoLens.Tag 7, endingSymbolStack__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StackGraphPath'_unknownFields
        (\ x__ y__ -> x__ {_StackGraphPath'_unknownFields = y__})
  defMessage
    = StackGraphPath'_constructor
        {_StackGraphPath'startingSymbolStack = Data.Vector.Generic.empty,
         _StackGraphPath'startingScopeStackSize = Data.ProtoLens.fieldDefault,
         _StackGraphPath'from = Data.ProtoLens.fieldDefault,
         _StackGraphPath'edges = Data.ProtoLens.fieldDefault,
         _StackGraphPath'to = Data.ProtoLens.fieldDefault,
         _StackGraphPath'endingScopeStack = Data.Vector.Generic.empty,
         _StackGraphPath'endingSymbolStack = Data.Vector.Generic.empty,
         _StackGraphPath'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StackGraphPath
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
                   -> Data.ProtoLens.Encoding.Bytes.Parser StackGraphPath
        loop
          x
          mutable'endingScopeStack
          mutable'endingSymbolStack
          mutable'startingSymbolStack
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'endingScopeStack <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'endingScopeStack)
                      frozen'endingSymbolStack <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                    (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                       mutable'endingSymbolStack)
                      frozen'startingSymbolStack <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                      (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                         mutable'startingSymbolStack)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'endingScopeStack")
                              frozen'endingScopeStack
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'endingSymbolStack")
                                 frozen'endingSymbolStack
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'startingSymbolStack")
                                    frozen'startingSymbolStack
                                    x))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "starting_symbol_stack"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'startingSymbolStack y)
                                loop x mutable'endingScopeStack mutable'endingSymbolStack v
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "starting_scope_stack_size"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"startingScopeStackSize") y x)
                                  mutable'endingScopeStack
                                  mutable'endingSymbolStack
                                  mutable'startingSymbolStack
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "from"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"from") y x)
                                  mutable'endingScopeStack
                                  mutable'endingSymbolStack
                                  mutable'startingSymbolStack
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "edges"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"edges") y x)
                                  mutable'endingScopeStack
                                  mutable'endingSymbolStack
                                  mutable'startingSymbolStack
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "to"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"to") y x)
                                  mutable'endingScopeStack
                                  mutable'endingSymbolStack
                                  mutable'startingSymbolStack
                        48
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "ending_scope_stack"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'endingScopeStack y)
                                loop x v mutable'endingSymbolStack mutable'startingSymbolStack
                        50
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "ending_scope_stack"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'endingScopeStack)
                                loop x y mutable'endingSymbolStack mutable'startingSymbolStack
                        58
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                        Data.ProtoLens.Encoding.Bytes.getBytes
                                                          (Prelude.fromIntegral len)
                                            Data.ProtoLens.Encoding.Bytes.runEither
                                              (case Data.Text.Encoding.decodeUtf8' value of
                                                 (Prelude.Left err)
                                                   -> Prelude.Left (Prelude.show err)
                                                 (Prelude.Right r) -> Prelude.Right r))
                                        "ending_symbol_stack"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'endingSymbolStack y)
                                loop x mutable'endingScopeStack v mutable'startingSymbolStack
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'endingScopeStack
                                  mutable'endingSymbolStack
                                  mutable'startingSymbolStack
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'endingScopeStack <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              mutable'endingSymbolStack <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             Data.ProtoLens.Encoding.Growing.new
              mutable'startingSymbolStack <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage
                mutable'endingScopeStack
                mutable'endingSymbolStack
                mutable'startingSymbolStack)
          "StackGraphPath"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.Text.Encoding.encodeUtf8
                           _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'startingSymbolStack") _x))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"startingScopeStackSize") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"from") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (let
                         _v = Lens.Family2.view (Data.ProtoLens.Field.field @"edges") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8
                                  _v))
                      ((Data.Monoid.<>)
                         (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"to") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     Prelude.fromIntegral
                                     _v))
                         ((Data.Monoid.<>)
                            (let
                               p = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"vec'endingScopeStack") _x
                             in
                               if Data.Vector.Generic.null p then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                     ((\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                        (Data.ProtoLens.Encoding.Bytes.runBuilder
                                           (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 Prelude.fromIntegral)
                                              p))))
                            ((Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                  (\ _v
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                          ((Prelude..)
                                             (\ bs
                                                -> (Data.Monoid.<>)
                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                        (Prelude.fromIntegral
                                                           (Data.ByteString.length bs)))
                                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                             Data.Text.Encoding.encodeUtf8
                                             _v))
                                  (Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"vec'endingSymbolStack") _x))
                               (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                  (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))
instance Control.DeepSeq.NFData StackGraphPath where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StackGraphPath'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_StackGraphPath'startingSymbolStack x__)
                (Control.DeepSeq.deepseq
                   (_StackGraphPath'startingScopeStackSize x__)
                   (Control.DeepSeq.deepseq
                      (_StackGraphPath'from x__)
                      (Control.DeepSeq.deepseq
                         (_StackGraphPath'edges x__)
                         (Control.DeepSeq.deepseq
                            (_StackGraphPath'to x__)
                            (Control.DeepSeq.deepseq
                               (_StackGraphPath'endingScopeStack x__)
                               (Control.DeepSeq.deepseq
                                  (_StackGraphPath'endingSymbolStack x__) ())))))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.blobs' @:: Lens' StackGraphRequest [Blob]@
         * 'Proto.Semantic_Fields.vec'blobs' @:: Lens' StackGraphRequest (Data.Vector.Vector Blob)@ -}
data StackGraphRequest
  = StackGraphRequest'_constructor {_StackGraphRequest'blobs :: !(Data.Vector.Vector Blob),
                                    _StackGraphRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StackGraphRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StackGraphRequest "blobs" [Blob] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphRequest'blobs
           (\ x__ y__ -> x__ {_StackGraphRequest'blobs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphRequest "vec'blobs" (Data.Vector.Vector Blob) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphRequest'blobs
           (\ x__ y__ -> x__ {_StackGraphRequest'blobs = y__}))
        Prelude.id
instance Data.ProtoLens.Message StackGraphRequest where
  messageName _ = Data.Text.pack "github.semantic.StackGraphRequest"
  fieldsByTag
    = let
        blobs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "blobs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Blob)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"blobs")) ::
              Data.ProtoLens.FieldDescriptor StackGraphRequest
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, blobs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StackGraphRequest'_unknownFields
        (\ x__ y__ -> x__ {_StackGraphRequest'_unknownFields = y__})
  defMessage
    = StackGraphRequest'_constructor
        {_StackGraphRequest'blobs = Data.Vector.Generic.empty,
         _StackGraphRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StackGraphRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Blob
             -> Data.ProtoLens.Encoding.Bytes.Parser StackGraphRequest
        loop x mutable'blobs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'blobs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'blobs)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'blobs") frozen'blobs x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "blobs"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'blobs y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'blobs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'blobs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'blobs)
          "StackGraphRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'blobs") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StackGraphRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StackGraphRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StackGraphRequest'blobs x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.files' @:: Lens' StackGraphResponse [StackGraphFile]@
         * 'Proto.Semantic_Fields.vec'files' @:: Lens' StackGraphResponse (Data.Vector.Vector StackGraphFile)@ -}
data StackGraphResponse
  = StackGraphResponse'_constructor {_StackGraphResponse'files :: !(Data.Vector.Vector StackGraphFile),
                                     _StackGraphResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show StackGraphResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField StackGraphResponse "files" [StackGraphFile] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphResponse'files
           (\ x__ y__ -> x__ {_StackGraphResponse'files = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField StackGraphResponse "vec'files" (Data.Vector.Vector StackGraphFile) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphResponse'files
           (\ x__ y__ -> x__ {_StackGraphResponse'files = y__}))
        Prelude.id
instance Data.ProtoLens.Message StackGraphResponse where
  messageName _ = Data.Text.pack "github.semantic.StackGraphResponse"
  fieldsByTag
    = let
        files__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "files"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor StackGraphFile)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"files")) ::
              Data.ProtoLens.FieldDescriptor StackGraphResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, files__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _StackGraphResponse'_unknownFields
        (\ x__ y__ -> x__ {_StackGraphResponse'_unknownFields = y__})
  defMessage
    = StackGraphResponse'_constructor
        {_StackGraphResponse'files = Data.Vector.Generic.empty,
         _StackGraphResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          StackGraphResponse
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld StackGraphFile
             -> Data.ProtoLens.Encoding.Bytes.Parser StackGraphResponse
        loop x mutable'files
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'files)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'files") frozen'files x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "files"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'files y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'files
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'files <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'files)
          "StackGraphResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage
                           _v))
                (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'files") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData StackGraphResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_StackGraphResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_StackGraphResponse'files x__) ())
{- | Fields :
     
         * 'Proto.Semantic_Fields.symbol' @:: Lens' Symbol Data.Text.Text@
         * 'Proto.Semantic_Fields.kind' @:: Lens' Symbol Data.Text.Text@
         * 'Proto.Semantic_Fields.line' @:: Lens' Symbol Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' Symbol Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' Symbol (Prelude.Maybe Span)@
         * 'Proto.Semantic_Fields.docs' @:: Lens' Symbol Docstring@
         * 'Proto.Semantic_Fields.maybe'docs' @:: Lens' Symbol (Prelude.Maybe Docstring)@ -}
data Symbol
  = Symbol'_constructor {_Symbol'symbol :: !Data.Text.Text,
                         _Symbol'kind :: !Data.Text.Text,
                         _Symbol'line :: !Data.Text.Text,
                         _Symbol'span :: !(Prelude.Maybe Span),
                         _Symbol'docs :: !(Prelude.Maybe Docstring),
                         _Symbol'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Symbol where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Symbol "symbol" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'symbol (\ x__ y__ -> x__ {_Symbol'symbol = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "kind" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'kind (\ x__ y__ -> x__ {_Symbol'kind = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "line" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'line (\ x__ y__ -> x__ {_Symbol'line = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "span" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'span (\ x__ y__ -> x__ {_Symbol'span = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Symbol "maybe'span" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'span (\ x__ y__ -> x__ {_Symbol'span = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "docs" Docstring where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'docs (\ x__ y__ -> x__ {_Symbol'docs = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Symbol "maybe'docs" (Prelude.Maybe Docstring) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'docs (\ x__ y__ -> x__ {_Symbol'docs = y__}))
        Prelude.id
instance Data.ProtoLens.Message Symbol where
  messageName _ = Data.Text.pack "github.semantic.Symbol"
  fieldsByTag
    = let
        symbol__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "symbol"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"symbol")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        kind__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "kind"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"kind")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        line__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "line"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"line")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        docs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "docs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Docstring)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'docs")) ::
              Data.ProtoLens.FieldDescriptor Symbol
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
           (Data.ProtoLens.Tag 2, kind__field_descriptor),
           (Data.ProtoLens.Tag 3, line__field_descriptor),
           (Data.ProtoLens.Tag 4, span__field_descriptor),
           (Data.ProtoLens.Tag 5, docs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Symbol'_unknownFields
        (\ x__ y__ -> x__ {_Symbol'_unknownFields = y__})
  defMessage
    = Symbol'_constructor
        {_Symbol'symbol = Data.ProtoLens.fieldDefault,
         _Symbol'kind = Data.ProtoLens.fieldDefault,
         _Symbol'line = Data.ProtoLens.fieldDefault,
         _Symbol'span = Prelude.Nothing, _Symbol'docs = Prelude.Nothing,
         _Symbol'_unknownFields = []}
  parseMessage
    = let
        loop :: Symbol -> Data.ProtoLens.Encoding.Bytes.Parser Symbol
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "symbol"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"symbol") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "kind"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"kind") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "line"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"line") y x)
                        34
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "docs"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"docs") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Symbol"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"symbol") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"kind") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"line") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8
                               _v))
                   ((Data.Monoid.<>)
                      (case
                           Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'span") _x
                       of
                         Prelude.Nothing -> Data.Monoid.mempty
                         (Prelude.Just _v)
                           -> (Data.Monoid.<>)
                                (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                ((Prelude..)
                                   (\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                   Data.ProtoLens.encodeMessage
                                   _v))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'docs") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage
                                      _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData Symbol where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Symbol'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Symbol'symbol x__)
                (Control.DeepSeq.deepseq
                   (_Symbol'kind x__)
                   (Control.DeepSeq.deepseq
                      (_Symbol'line x__)
                      (Control.DeepSeq.deepseq
                         (_Symbol'span x__)
                         (Control.DeepSeq.deepseq (_Symbol'docs x__) ())))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.category' @:: Lens' TOCSummaryChange Data.Text.Text@
         * 'Proto.Semantic_Fields.term' @:: Lens' TOCSummaryChange Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' TOCSummaryChange Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' TOCSummaryChange (Prelude.Maybe Span)@
         * 'Proto.Semantic_Fields.changeType' @:: Lens' TOCSummaryChange ChangeType@ -}
data TOCSummaryChange
  = TOCSummaryChange'_constructor {_TOCSummaryChange'category :: !Data.Text.Text,
                                   _TOCSummaryChange'term :: !Data.Text.Text,
                                   _TOCSummaryChange'span :: !(Prelude.Maybe Span),
                                   _TOCSummaryChange'changeType :: !ChangeType,
                                   _TOCSummaryChange'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TOCSummaryChange where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TOCSummaryChange "category" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryChange'category
           (\ x__ y__ -> x__ {_TOCSummaryChange'category = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TOCSummaryChange "term" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryChange'term
           (\ x__ y__ -> x__ {_TOCSummaryChange'term = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TOCSummaryChange "span" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryChange'span
           (\ x__ y__ -> x__ {_TOCSummaryChange'span = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TOCSummaryChange "maybe'span" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryChange'span
           (\ x__ y__ -> x__ {_TOCSummaryChange'span = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TOCSummaryChange "changeType" ChangeType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryChange'changeType
           (\ x__ y__ -> x__ {_TOCSummaryChange'changeType = y__}))
        Prelude.id
instance Data.ProtoLens.Message TOCSummaryChange where
  messageName _ = Data.Text.pack "github.semantic.TOCSummaryChange"
  fieldsByTag
    = let
        category__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "category"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"category")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryChange
        term__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "term"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"term")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryChange
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryChange
        changeType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "change_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor ChangeType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"changeType")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryChange
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, category__field_descriptor),
           (Data.ProtoLens.Tag 2, term__field_descriptor),
           (Data.ProtoLens.Tag 3, span__field_descriptor),
           (Data.ProtoLens.Tag 4, changeType__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TOCSummaryChange'_unknownFields
        (\ x__ y__ -> x__ {_TOCSummaryChange'_unknownFields = y__})
  defMessage
    = TOCSummaryChange'_constructor
        {_TOCSummaryChange'category = Data.ProtoLens.fieldDefault,
         _TOCSummaryChange'term = Data.ProtoLens.fieldDefault,
         _TOCSummaryChange'span = Prelude.Nothing,
         _TOCSummaryChange'changeType = Data.ProtoLens.fieldDefault,
         _TOCSummaryChange'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TOCSummaryChange
          -> Data.ProtoLens.Encoding.Bytes.Parser TOCSummaryChange
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "category"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"category") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "term"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"term") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "change_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"changeType") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TOCSummaryChange"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"category") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"term") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'span") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"changeType") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                                  Prelude.fromEnum
                                  _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData TOCSummaryChange where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TOCSummaryChange'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TOCSummaryChange'category x__)
                (Control.DeepSeq.deepseq
                   (_TOCSummaryChange'term x__)
                   (Control.DeepSeq.deepseq
                      (_TOCSummaryChange'span x__)
                      (Control.DeepSeq.deepseq (_TOCSummaryChange'changeType x__) ()))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.error' @:: Lens' TOCSummaryError Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' TOCSummaryError Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' TOCSummaryError (Prelude.Maybe Span)@ -}
data TOCSummaryError
  = TOCSummaryError'_constructor {_TOCSummaryError'error :: !Data.Text.Text,
                                  _TOCSummaryError'span :: !(Prelude.Maybe Span),
                                  _TOCSummaryError'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TOCSummaryError where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TOCSummaryError "error" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryError'error
           (\ x__ y__ -> x__ {_TOCSummaryError'error = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TOCSummaryError "span" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryError'span
           (\ x__ y__ -> x__ {_TOCSummaryError'span = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TOCSummaryError "maybe'span" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryError'span
           (\ x__ y__ -> x__ {_TOCSummaryError'span = y__}))
        Prelude.id
instance Data.ProtoLens.Message TOCSummaryError where
  messageName _ = Data.Text.pack "github.semantic.TOCSummaryError"
  fieldsByTag
    = let
        error__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"error")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryError
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryError
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, error__field_descriptor),
           (Data.ProtoLens.Tag 2, span__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TOCSummaryError'_unknownFields
        (\ x__ y__ -> x__ {_TOCSummaryError'_unknownFields = y__})
  defMessage
    = TOCSummaryError'_constructor
        {_TOCSummaryError'error = Data.ProtoLens.fieldDefault,
         _TOCSummaryError'span = Prelude.Nothing,
         _TOCSummaryError'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TOCSummaryError
          -> Data.ProtoLens.Encoding.Bytes.Parser TOCSummaryError
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "error"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"error") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TOCSummaryError"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"error") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (case
                     Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'span") _x
                 of
                   Prelude.Nothing -> Data.Monoid.mempty
                   (Prelude.Just _v)
                     -> (Data.Monoid.<>)
                          (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                          ((Prelude..)
                             (\ bs
                                -> (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                        (Prelude.fromIntegral (Data.ByteString.length bs)))
                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                             Data.ProtoLens.encodeMessage
                             _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TOCSummaryError where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TOCSummaryError'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TOCSummaryError'error x__)
                (Control.DeepSeq.deepseq (_TOCSummaryError'span x__) ()))
{- | Fields :
     
         * 'Proto.Semantic_Fields.path' @:: Lens' TOCSummaryFile Data.Text.Text@
         * 'Proto.Semantic_Fields.language' @:: Lens' TOCSummaryFile Data.Text.Text@
         * 'Proto.Semantic_Fields.changes' @:: Lens' TOCSummaryFile [TOCSummaryChange]@
         * 'Proto.Semantic_Fields.vec'changes' @:: Lens' TOCSummaryFile (Data.Vector.Vector TOCSummaryChange)@
         * 'Proto.Semantic_Fields.errors' @:: Lens' TOCSummaryFile [TOCSummaryError]@
         * 'Proto.Semantic_Fields.vec'errors' @:: Lens' TOCSummaryFile (Data.Vector.Vector TOCSummaryError)@ -}
data TOCSummaryFile
  = TOCSummaryFile'_constructor {_TOCSummaryFile'path :: !Data.Text.Text,
                                 _TOCSummaryFile'language :: !Data.Text.Text,
                                 _TOCSummaryFile'changes :: !(Data.Vector.Vector TOCSummaryChange),
                                 _TOCSummaryFile'errors :: !(Data.Vector.Vector TOCSummaryError),
                                 _TOCSummaryFile'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TOCSummaryFile where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TOCSummaryFile "path" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryFile'path
           (\ x__ y__ -> x__ {_TOCSummaryFile'path = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TOCSummaryFile "language" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryFile'language
           (\ x__ y__ -> x__ {_TOCSummaryFile'language = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TOCSummaryFile "changes" [TOCSummaryChange] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryFile'changes
           (\ x__ y__ -> x__ {_TOCSummaryFile'changes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TOCSummaryFile "vec'changes" (Data.Vector.Vector TOCSummaryChange) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryFile'changes
           (\ x__ y__ -> x__ {_TOCSummaryFile'changes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TOCSummaryFile "errors" [TOCSummaryError] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryFile'errors
           (\ x__ y__ -> x__ {_TOCSummaryFile'errors = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField TOCSummaryFile "vec'errors" (Data.Vector.Vector TOCSummaryError) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TOCSummaryFile'errors
           (\ x__ y__ -> x__ {_TOCSummaryFile'errors = y__}))
        Prelude.id
instance Data.ProtoLens.Message TOCSummaryFile where
  messageName _ = Data.Text.pack "github.semantic.TOCSummaryFile"
  fieldsByTag
    = let
        path__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "path"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"path")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryFile
        language__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "language"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"language")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryFile
        changes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "changes"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TOCSummaryChange)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"changes")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryFile
        errors__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "errors"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor TOCSummaryError)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"errors")) ::
              Data.ProtoLens.FieldDescriptor TOCSummaryFile
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, path__field_descriptor),
           (Data.ProtoLens.Tag 2, language__field_descriptor),
           (Data.ProtoLens.Tag 3, changes__field_descriptor),
           (Data.ProtoLens.Tag 4, errors__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TOCSummaryFile'_unknownFields
        (\ x__ y__ -> x__ {_TOCSummaryFile'_unknownFields = y__})
  defMessage
    = TOCSummaryFile'_constructor
        {_TOCSummaryFile'path = Data.ProtoLens.fieldDefault,
         _TOCSummaryFile'language = Data.ProtoLens.fieldDefault,
         _TOCSummaryFile'changes = Data.Vector.Generic.empty,
         _TOCSummaryFile'errors = Data.Vector.Generic.empty,
         _TOCSummaryFile'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TOCSummaryFile
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TOCSummaryChange
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld TOCSummaryError
                -> Data.ProtoLens.Encoding.Bytes.Parser TOCSummaryFile
        loop x mutable'changes mutable'errors
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'changes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'changes)
                      frozen'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'errors)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields
                           (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'changes")
                              frozen'changes
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'errors") frozen'errors x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "path"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"path") y x)
                                  mutable'changes
                                  mutable'errors
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "language"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"language") y x)
                                  mutable'changes
                                  mutable'errors
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "changes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'changes y)
                                loop x v mutable'errors
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "errors"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'errors y)
                                loop x mutable'changes v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'changes
                                  mutable'errors
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'changes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              mutable'errors <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'changes mutable'errors)
          "TOCSummaryFile"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"path") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((Prelude..)
                         (\ bs
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                    (Prelude.fromIntegral (Data.ByteString.length bs)))
                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         Data.Text.Encoding.encodeUtf8
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"language") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage
                                 _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'changes") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage
                                    _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'errors") _x))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData TOCSummaryFile where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TOCSummaryFile'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TOCSummaryFile'path x__)
                (Control.DeepSeq.deepseq
                   (_TOCSummaryFile'language x__)
                   (Control.DeepSeq.deepseq
                      (_TOCSummaryFile'changes x__)
                      (Control.DeepSeq.deepseq (_TOCSummaryFile'errors x__) ()))))
{- | Fields :
     
         * 'Proto.Semantic_Fields.source' @:: Lens' TermEdge Data.Int.Int32@
         * 'Proto.Semantic_Fields.target' @:: Lens' TermEdge Data.Int.Int32@ -}
data TermEdge
  = TermEdge'_constructor {_TermEdge'source :: !Data.Int.Int32,
                           _TermEdge'target :: !Data.Int.Int32,
                           _TermEdge'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TermEdge where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TermEdge "source" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TermEdge'source (\ x__ y__ -> x__ {_TermEdge'source = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TermEdge "target" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TermEdge'target (\ x__ y__ -> x__ {_TermEdge'target = y__}))
        Prelude.id
instance Data.ProtoLens.Message TermEdge where
  messageName _ = Data.Text.pack "github.semantic.TermEdge"
  fieldsByTag
    = let
        source__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "source"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"source")) ::
              Data.ProtoLens.FieldDescriptor TermEdge
        target__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "target"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"target")) ::
              Data.ProtoLens.FieldDescriptor TermEdge
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, source__field_descriptor),
           (Data.ProtoLens.Tag 2, target__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TermEdge'_unknownFields
        (\ x__ y__ -> x__ {_TermEdge'_unknownFields = y__})
  defMessage
    = TermEdge'_constructor
        {_TermEdge'source = Data.ProtoLens.fieldDefault,
         _TermEdge'target = Data.ProtoLens.fieldDefault,
         _TermEdge'_unknownFields = []}
  parseMessage
    = let
        loop :: TermEdge -> Data.ProtoLens.Encoding.Bytes.Parser TermEdge
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "source"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"source") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "target"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"target") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TermEdge"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"source") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"target") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData TermEdge where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TermEdge'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TermEdge'source x__)
                (Control.DeepSeq.deepseq (_TermEdge'target x__) ()))
{- | Fields :
     
         * 'Proto.Semantic_Fields.vertexId' @:: Lens' TermVertex Data.Int.Int32@
         * 'Proto.Semantic_Fields.term' @:: Lens' TermVertex Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' TermVertex Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' TermVertex (Prelude.Maybe Span)@ -}
data TermVertex
  = TermVertex'_constructor {_TermVertex'vertexId :: !Data.Int.Int32,
                             _TermVertex'term :: !Data.Text.Text,
                             _TermVertex'span :: !(Prelude.Maybe Span),
                             _TermVertex'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving (Prelude.Eq, Prelude.Ord)
instance Prelude.Show TermVertex where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField TermVertex "vertexId" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TermVertex'vertexId
           (\ x__ y__ -> x__ {_TermVertex'vertexId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TermVertex "term" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TermVertex'term (\ x__ y__ -> x__ {_TermVertex'term = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField TermVertex "span" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TermVertex'span (\ x__ y__ -> x__ {_TermVertex'span = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField TermVertex "maybe'span" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _TermVertex'span (\ x__ y__ -> x__ {_TermVertex'span = y__}))
        Prelude.id
instance Data.ProtoLens.Message TermVertex where
  messageName _ = Data.Text.pack "github.semantic.TermVertex"
  fieldsByTag
    = let
        vertexId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "vertex_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"vertexId")) ::
              Data.ProtoLens.FieldDescriptor TermVertex
        term__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "term"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"term")) ::
              Data.ProtoLens.FieldDescriptor TermVertex
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor TermVertex
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, vertexId__field_descriptor),
           (Data.ProtoLens.Tag 2, term__field_descriptor),
           (Data.ProtoLens.Tag 3, span__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _TermVertex'_unknownFields
        (\ x__ y__ -> x__ {_TermVertex'_unknownFields = y__})
  defMessage
    = TermVertex'_constructor
        {_TermVertex'vertexId = Data.ProtoLens.fieldDefault,
         _TermVertex'term = Data.ProtoLens.fieldDefault,
         _TermVertex'span = Prelude.Nothing,
         _TermVertex'_unknownFields = []}
  parseMessage
    = let
        loop ::
          TermVertex -> Data.ProtoLens.Encoding.Bytes.Parser TermVertex
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "vertex_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"vertexId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do value <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                                       Data.ProtoLens.Encoding.Bytes.getBytes
                                                         (Prelude.fromIntegral len)
                                           Data.ProtoLens.Encoding.Bytes.runEither
                                             (case Data.Text.Encoding.decodeUtf8' value of
                                                (Prelude.Left err)
                                                  -> Prelude.Left (Prelude.show err)
                                                (Prelude.Right r) -> Prelude.Right r))
                                       "term"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"term") y x)
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "TermVertex"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"vertexId") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"term") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8
                            _v))
                ((Data.Monoid.<>)
                   (case
                        Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'span") _x
                    of
                      Prelude.Nothing -> Data.Monoid.mempty
                      (Prelude.Just _v)
                        -> (Data.Monoid.<>)
                             (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                             ((Prelude..)
                                (\ bs
                                   -> (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                           (Prelude.fromIntegral (Data.ByteString.length bs)))
                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                Data.ProtoLens.encodeMessage
                                _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData TermVertex where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_TermVertex'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_TermVertex'vertexId x__)
                (Control.DeepSeq.deepseq
                   (_TermVertex'term x__)
                   (Control.DeepSeq.deepseq (_TermVertex'span x__) ())))