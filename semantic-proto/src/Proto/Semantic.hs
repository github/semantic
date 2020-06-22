{- This file was auto-generated from semantic.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Semantic (
        Blob(), DeletedTerm(), DiffTreeEdge(), DiffTreeFileGraph(),
        DiffTreeGraphResponse(), DiffTreeVertex(),
        DiffTreeVertex'DiffTerm(..), _DiffTreeVertex'Deleted,
        _DiffTreeVertex'Inserted, _DiffTreeVertex'Replaced,
        _DiffTreeVertex'Merged, Docstring(), File(), InsertedTerm(),
        MergedTerm(), NodeType(..), NodeType(), NodeType'UnrecognizedValue,
        ParseError(), ParseTreeFileGraph(), ParseTreeGraphResponse(),
        ParseTreeRequest(), ParseTreeSymbolResponse(), PingRequest(),
        PingResponse(), Position(), ReplacedTerm(), Span(),
        StackGraphFile(), StackGraphNode(), StackGraphPath(),
        StackGraphRequest(), StackGraphResponse(), Symbol(),
        SyntaxType(..), SyntaxType(), SyntaxType'UnrecognizedValue,
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\EOTBlob\DC2\CAN\n\
      \\acontent\CAN\SOH \SOH(\tR\acontent\DC2\DC2\n\
      \\EOTpath\CAN\STX \SOH(\tR\EOTpath\DC2\SUB\n\
      \\blanguage\CAN\ETX \SOH(\tR\blanguage"
  packedFileDescriptor _ = packedFileDescriptor
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
     
         * 'Proto.Semantic_Fields.term' @:: Lens' DeletedTerm Data.Text.Text@
         * 'Proto.Semantic_Fields.span' @:: Lens' DeletedTerm Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' DeletedTerm (Prelude.Maybe Span)@ -}
data DeletedTerm
  = DeletedTerm'_constructor {_DeletedTerm'term :: !Data.Text.Text,
                              _DeletedTerm'span :: !(Prelude.Maybe Span),
                              _DeletedTerm'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\vDeletedTerm\DC2\DC2\n\
      \\EOTterm\CAN\SOH \SOH(\tR\EOTterm\DC2)\n\
      \\EOTspan\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\EOTspan"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\fDiffTreeEdge\DC2\SYN\n\
      \\ACKsource\CAN\SOH \SOH(\ENQR\ACKsource\DC2\SYN\n\
      \\ACKtarget\CAN\STX \SOH(\ENQR\ACKtarget"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\DC1DiffTreeFileGraph\DC2\DC2\n\
      \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
      \\blanguage\CAN\STX \SOH(\tR\blanguage\DC2;\n\
      \\bvertices\CAN\ETX \ETX(\v2\US.github.semantic.DiffTreeVertexR\bvertices\DC23\n\
      \\ENQedges\CAN\EOT \ETX(\v2\GS.github.semantic.DiffTreeEdgeR\ENQedges\DC23\n\
      \\ACKerrors\CAN\ENQ \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\NAKDiffTreeGraphResponse\DC28\n\
      \\ENQfiles\CAN\SOH \ETX(\v2\".github.semantic.DiffTreeFileGraphR\ENQfiles"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\SODiffTreeVertex\DC2$\n\
      \\SOdiff_vertex_id\CAN\SOH \SOH(\ENQR\fdiffVertexId\DC28\n\
      \\adeleted\CAN\STX \SOH(\v2\FS.github.semantic.DeletedTermH\NULR\adeleted\DC2;\n\
      \\binserted\CAN\ETX \SOH(\v2\GS.github.semantic.InsertedTermH\NULR\binserted\DC2;\n\
      \\breplaced\CAN\EOT \SOH(\v2\GS.github.semantic.ReplacedTermH\NULR\breplaced\DC25\n\
      \\ACKmerged\CAN\ENQ \SOH(\v2\ESC.github.semantic.MergedTermH\NULR\ACKmergedB\v\n\
      \\tdiff_term"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\tDocstring\DC2\FS\n\
      \\tdocstring\CAN\SOH \SOH(\tR\tdocstring"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\EOTFile\DC2\DC2\n\
      \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
      \\blanguage\CAN\STX \SOH(\tR\blanguage\DC21\n\
      \\asymbols\CAN\ETX \ETX(\v2\ETB.github.semantic.SymbolR\asymbols\DC23\n\
      \\ACKerrors\CAN\EOT \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors\DC2\EM\n\
      \\bblob_oid\CAN\ENQ \SOH(\tR\ablobOid"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\fInsertedTerm\DC2\DC2\n\
      \\EOTterm\CAN\SOH \SOH(\tR\EOTterm\DC2)\n\
      \\EOTspan\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\EOTspan"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\n\
      \MergedTerm\DC2\DC2\n\
      \\EOTterm\CAN\SOH \SOH(\tR\EOTterm\DC26\n\
      \\vbefore_span\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\n\
      \beforeSpan\DC24\n\
      \\n\
      \after_span\CAN\ETX \SOH(\v2\NAK.github.semantic.SpanR\tafterSpan"
  packedFileDescriptor _ = packedFileDescriptor
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
newtype NodeType'UnrecognizedValue
  = NodeType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data NodeType
  = UNKNOWN_NODE |
    ROOT_SCOPE |
    JUMP_TO_SCOPE |
    EXPORTED_SCOPE |
    DEFINITION |
    REFERENCE |
    NodeType'Unrecognized !NodeType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum NodeType where
  maybeToEnum 0 = Prelude.Just UNKNOWN_NODE
  maybeToEnum 1 = Prelude.Just ROOT_SCOPE
  maybeToEnum 2 = Prelude.Just JUMP_TO_SCOPE
  maybeToEnum 3 = Prelude.Just EXPORTED_SCOPE
  maybeToEnum 4 = Prelude.Just DEFINITION
  maybeToEnum 5 = Prelude.Just REFERENCE
  maybeToEnum k
    = Prelude.Just
        (NodeType'Unrecognized
           (NodeType'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UNKNOWN_NODE = "UNKNOWN_NODE"
  showEnum ROOT_SCOPE = "ROOT_SCOPE"
  showEnum JUMP_TO_SCOPE = "JUMP_TO_SCOPE"
  showEnum EXPORTED_SCOPE = "EXPORTED_SCOPE"
  showEnum DEFINITION = "DEFINITION"
  showEnum REFERENCE = "REFERENCE"
  showEnum (NodeType'Unrecognized (NodeType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UNKNOWN_NODE" = Prelude.Just UNKNOWN_NODE
    | (Prelude.==) k "ROOT_SCOPE" = Prelude.Just ROOT_SCOPE
    | (Prelude.==) k "JUMP_TO_SCOPE" = Prelude.Just JUMP_TO_SCOPE
    | (Prelude.==) k "EXPORTED_SCOPE" = Prelude.Just EXPORTED_SCOPE
    | (Prelude.==) k "DEFINITION" = Prelude.Just DEFINITION
    | (Prelude.==) k "REFERENCE" = Prelude.Just REFERENCE
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded NodeType where
  minBound = UNKNOWN_NODE
  maxBound = REFERENCE
instance Prelude.Enum NodeType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum NodeType: " (Prelude.show k__)))
        Prelude.id
        (Data.ProtoLens.maybeToEnum k__)
  fromEnum UNKNOWN_NODE = 0
  fromEnum ROOT_SCOPE = 1
  fromEnum JUMP_TO_SCOPE = 2
  fromEnum EXPORTED_SCOPE = 3
  fromEnum DEFINITION = 4
  fromEnum REFERENCE = 5
  fromEnum (NodeType'Unrecognized (NodeType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ REFERENCE
    = Prelude.error
        "NodeType.succ: bad argument REFERENCE. This value would be out of bounds."
  succ UNKNOWN_NODE = ROOT_SCOPE
  succ ROOT_SCOPE = JUMP_TO_SCOPE
  succ JUMP_TO_SCOPE = EXPORTED_SCOPE
  succ EXPORTED_SCOPE = DEFINITION
  succ DEFINITION = REFERENCE
  succ (NodeType'Unrecognized _)
    = Prelude.error "NodeType.succ: bad argument: unrecognized value"
  pred UNKNOWN_NODE
    = Prelude.error
        "NodeType.pred: bad argument UNKNOWN_NODE. This value would be out of bounds."
  pred ROOT_SCOPE = UNKNOWN_NODE
  pred JUMP_TO_SCOPE = ROOT_SCOPE
  pred EXPORTED_SCOPE = JUMP_TO_SCOPE
  pred DEFINITION = EXPORTED_SCOPE
  pred REFERENCE = DEFINITION
  pred (NodeType'Unrecognized _)
    = Prelude.error "NodeType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault NodeType where
  fieldDefault = UNKNOWN_NODE
instance Control.DeepSeq.NFData NodeType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Semantic_Fields.error' @:: Lens' ParseError Data.Text.Text@ -}
data ParseError
  = ParseError'_constructor {_ParseError'error :: !Data.Text.Text,
                             _ParseError'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\n\
      \ParseError\DC2\DC4\n\
      \\ENQerror\CAN\SOH \SOH(\tR\ENQerror"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\DC2ParseTreeFileGraph\DC2\DC2\n\
      \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
      \\blanguage\CAN\STX \SOH(\tR\blanguage\DC27\n\
      \\bvertices\CAN\ETX \ETX(\v2\ESC.github.semantic.TermVertexR\bvertices\DC2/\n\
      \\ENQedges\CAN\EOT \ETX(\v2\EM.github.semantic.TermEdgeR\ENQedges\DC23\n\
      \\ACKerrors\CAN\ENQ \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\SYNParseTreeGraphResponse\DC29\n\
      \\ENQfiles\CAN\SOH \ETX(\v2#.github.semantic.ParseTreeFileGraphR\ENQfiles"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\DLEParseTreeRequest\DC2+\n\
      \\ENQblobs\CAN\SOH \ETX(\v2\NAK.github.semantic.BlobR\ENQblobs"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\ETBParseTreeSymbolResponse\DC2+\n\
      \\ENQfiles\CAN\SOH \ETX(\v2\NAK.github.semantic.FileR\ENQfiles"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\vPingRequest\DC2\CAN\n\
      \\aservice\CAN\SOH \SOH(\tR\aservice"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\fPingResponse\DC2\SYN\n\
      \\ACKstatus\CAN\SOH \SOH(\tR\ACKstatus\DC2\SUB\n\
      \\bhostname\CAN\STX \SOH(\tR\bhostname\DC2\FS\n\
      \\ttimestamp\CAN\ETX \SOH(\tR\ttimestamp\DC2\DLE\n\
      \\ETXsha\CAN\EOT \SOH(\tR\ETXsha"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\bPosition\DC2\DC2\n\
      \\EOTline\CAN\SOH \SOH(\ENQR\EOTline\DC2\SYN\n\
      \\ACKcolumn\CAN\STX \SOH(\ENQR\ACKcolumn"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\fReplacedTerm\DC2\US\n\
      \\vbefore_term\CAN\SOH \SOH(\tR\n\
      \beforeTerm\DC26\n\
      \\vbefore_span\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\n\
      \beforeSpan\DC2\GS\n\
      \\n\
      \after_term\CAN\ETX \SOH(\tR\tafterTerm\DC24\n\
      \\n\
      \after_span\CAN\EOT \SOH(\v2\NAK.github.semantic.SpanR\tafterSpan"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\EOTSpan\DC2/\n\
      \\ENQstart\CAN\SOH \SOH(\v2\EM.github.semantic.PositionR\ENQstart\DC2+\n\
      \\ETXend\CAN\STX \SOH(\v2\EM.github.semantic.PositionR\ETXend"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\SOStackGraphFile\DC2\DC2\n\
      \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
      \\blanguage\CAN\STX \SOH(\tR\blanguage\DC25\n\
      \\ENQnodes\CAN\ETX \ETX(\v2\US.github.semantic.StackGraphNodeR\ENQnodes\DC25\n\
      \\ENQpaths\CAN\EOT \ETX(\v2\US.github.semantic.StackGraphPathR\ENQpaths\DC23\n\
      \\ACKerrors\CAN\ENQ \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors"
  packedFileDescriptor _ = packedFileDescriptor
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
         * 'Proto.Semantic_Fields.span' @:: Lens' StackGraphNode Span@
         * 'Proto.Semantic_Fields.maybe'span' @:: Lens' StackGraphNode (Prelude.Maybe Span)@
         * 'Proto.Semantic_Fields.syntaxType' @:: Lens' StackGraphNode SyntaxType@
         * 'Proto.Semantic_Fields.nodeType' @:: Lens' StackGraphNode NodeType@ -}
data StackGraphNode
  = StackGraphNode'_constructor {_StackGraphNode'id :: !Data.Int.Int64,
                                 _StackGraphNode'name :: !Data.Text.Text,
                                 _StackGraphNode'line :: !Data.Text.Text,
                                 _StackGraphNode'span :: !(Prelude.Maybe Span),
                                 _StackGraphNode'syntaxType :: !SyntaxType,
                                 _StackGraphNode'nodeType :: !NodeType,
                                 _StackGraphNode'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
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
instance Data.ProtoLens.Field.HasField StackGraphNode "syntaxType" SyntaxType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'syntaxType
           (\ x__ y__ -> x__ {_StackGraphNode'syntaxType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField StackGraphNode "nodeType" NodeType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _StackGraphNode'nodeType
           (\ x__ y__ -> x__ {_StackGraphNode'nodeType = y__}))
        Prelude.id
instance Data.ProtoLens.Message StackGraphNode where
  messageName _ = Data.Text.pack "github.semantic.StackGraphNode"
  packedMessageDescriptor _
    = "\n\
      \\SOStackGraphNode\DC2\SO\n\
      \\STXid\CAN\SOH \SOH(\ETXR\STXid\DC2\DC2\n\
      \\EOTname\CAN\STX \SOH(\tR\EOTname\DC2\DC2\n\
      \\EOTline\CAN\ETX \SOH(\tR\EOTline\DC2)\n\
      \\EOTspan\CAN\EOT \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\DC2<\n\
      \\vsyntax_type\CAN\ENQ \SOH(\SO2\ESC.github.semantic.SyntaxTypeR\n\
      \syntaxType\DC26\n\
      \\tnode_type\CAN\ACK \SOH(\SO2\EM.github.semantic.NodeTypeR\bnodeType"
  packedFileDescriptor _ = packedFileDescriptor
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
        span__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'span")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
        syntaxType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "syntax_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor SyntaxType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"syntaxType")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
        nodeType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "node_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor NodeType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nodeType")) ::
              Data.ProtoLens.FieldDescriptor StackGraphNode
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, id__field_descriptor),
           (Data.ProtoLens.Tag 2, name__field_descriptor),
           (Data.ProtoLens.Tag 3, line__field_descriptor),
           (Data.ProtoLens.Tag 4, span__field_descriptor),
           (Data.ProtoLens.Tag 5, syntaxType__field_descriptor),
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
         _StackGraphNode'span = Prelude.Nothing,
         _StackGraphNode'syntaxType = Data.ProtoLens.fieldDefault,
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
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "span"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"span") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "syntax_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"syntaxType") y x)
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
                         (let
                            _v
                              = Lens.Family2.view (Data.ProtoLens.Field.field @"syntaxType") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt
                                        Prelude.fromIntegral)
                                     Prelude.fromEnum
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
                         (_StackGraphNode'span x__)
                         (Control.DeepSeq.deepseq
                            (_StackGraphNode'syntaxType x__)
                            (Control.DeepSeq.deepseq (_StackGraphNode'nodeType x__) ()))))))
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\SOStackGraphPath\DC22\n\
      \\NAKstarting_symbol_stack\CAN\SOH \ETX(\tR\DC3startingSymbolStack\DC29\n\
      \\EMstarting_scope_stack_size\CAN\STX \SOH(\ETXR\SYNstartingScopeStackSize\DC2\DC2\n\
      \\EOTfrom\CAN\ETX \SOH(\ETXR\EOTfrom\DC2\DC4\n\
      \\ENQedges\CAN\EOT \SOH(\tR\ENQedges\DC2\SO\n\
      \\STXto\CAN\ENQ \SOH(\ETXR\STXto\DC2,\n\
      \\DC2ending_scope_stack\CAN\ACK \ETX(\ETXR\DLEendingScopeStack\DC2.\n\
      \\DC3ending_symbol_stack\CAN\a \ETX(\tR\DC1endingSymbolStack"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\DC1StackGraphRequest\DC2+\n\
      \\ENQblobs\CAN\SOH \ETX(\v2\NAK.github.semantic.BlobR\ENQblobs"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\DC2StackGraphResponse\DC25\n\
      \\ENQfiles\CAN\SOH \ETX(\v2\US.github.semantic.StackGraphFileR\ENQfiles"
  packedFileDescriptor _ = packedFileDescriptor
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
         * 'Proto.Semantic_Fields.maybe'docs' @:: Lens' Symbol (Prelude.Maybe Docstring)@
         * 'Proto.Semantic_Fields.nodeType' @:: Lens' Symbol NodeType@
         * 'Proto.Semantic_Fields.syntaxType' @:: Lens' Symbol SyntaxType@ -}
data Symbol
  = Symbol'_constructor {_Symbol'symbol :: !Data.Text.Text,
                         _Symbol'kind :: !Data.Text.Text,
                         _Symbol'line :: !Data.Text.Text,
                         _Symbol'span :: !(Prelude.Maybe Span),
                         _Symbol'docs :: !(Prelude.Maybe Docstring),
                         _Symbol'nodeType :: !NodeType,
                         _Symbol'syntaxType :: !SyntaxType,
                         _Symbol'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
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
instance Data.ProtoLens.Field.HasField Symbol "nodeType" NodeType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'nodeType (\ x__ y__ -> x__ {_Symbol'nodeType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "syntaxType" SyntaxType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'syntaxType (\ x__ y__ -> x__ {_Symbol'syntaxType = y__}))
        Prelude.id
instance Data.ProtoLens.Message Symbol where
  messageName _ = Data.Text.pack "github.semantic.Symbol"
  packedMessageDescriptor _
    = "\n\
      \\ACKSymbol\DC2\SYN\n\
      \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2\DC2\n\
      \\EOTkind\CAN\STX \SOH(\tR\EOTkind\DC2\DC2\n\
      \\EOTline\CAN\ETX \SOH(\tR\EOTline\DC2)\n\
      \\EOTspan\CAN\EOT \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\DC2.\n\
      \\EOTdocs\CAN\ENQ \SOH(\v2\SUB.github.semantic.DocstringR\EOTdocs\DC26\n\
      \\tnode_type\CAN\ACK \SOH(\SO2\EM.github.semantic.NodeTypeR\bnodeType\DC2<\n\
      \\vsyntax_type\CAN\a \SOH(\SO2\ESC.github.semantic.SyntaxTypeR\n\
      \syntaxType"
  packedFileDescriptor _ = packedFileDescriptor
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
        nodeType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "node_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor NodeType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"nodeType")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        syntaxType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "syntax_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor SyntaxType)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"syntaxType")) ::
              Data.ProtoLens.FieldDescriptor Symbol
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
           (Data.ProtoLens.Tag 2, kind__field_descriptor),
           (Data.ProtoLens.Tag 3, line__field_descriptor),
           (Data.ProtoLens.Tag 4, span__field_descriptor),
           (Data.ProtoLens.Tag 5, docs__field_descriptor),
           (Data.ProtoLens.Tag 6, nodeType__field_descriptor),
           (Data.ProtoLens.Tag 7, syntaxType__field_descriptor)]
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
         _Symbol'nodeType = Data.ProtoLens.fieldDefault,
         _Symbol'syntaxType = Data.ProtoLens.fieldDefault,
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
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "syntax_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"syntaxType") y x)
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
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"syntaxType") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                                        ((Prelude..)
                                           ((Prelude..)
                                              Data.ProtoLens.Encoding.Bytes.putVarInt
                                              Prelude.fromIntegral)
                                           Prelude.fromEnum
                                           _v))
                               (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                  (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))
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
                         (Control.DeepSeq.deepseq
                            (_Symbol'docs x__)
                            (Control.DeepSeq.deepseq
                               (_Symbol'nodeType x__)
                               (Control.DeepSeq.deepseq (_Symbol'syntaxType x__) ())))))))
newtype SyntaxType'UnrecognizedValue
  = SyntaxType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data SyntaxType
  = UNKNOWN_SYNTAX |
    FUNCTION |
    METHOD |
    CLASS |
    MODULE |
    CALL |
    TYPE |
    INTERFACE |
    IMPLEMENTATION |
    SyntaxType'Unrecognized !SyntaxType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum SyntaxType where
  maybeToEnum 0 = Prelude.Just UNKNOWN_SYNTAX
  maybeToEnum 1 = Prelude.Just FUNCTION
  maybeToEnum 2 = Prelude.Just METHOD
  maybeToEnum 3 = Prelude.Just CLASS
  maybeToEnum 4 = Prelude.Just MODULE
  maybeToEnum 5 = Prelude.Just CALL
  maybeToEnum 6 = Prelude.Just TYPE
  maybeToEnum 7 = Prelude.Just INTERFACE
  maybeToEnum 8 = Prelude.Just IMPLEMENTATION
  maybeToEnum k
    = Prelude.Just
        (SyntaxType'Unrecognized
           (SyntaxType'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum UNKNOWN_SYNTAX = "UNKNOWN_SYNTAX"
  showEnum FUNCTION = "FUNCTION"
  showEnum METHOD = "METHOD"
  showEnum CLASS = "CLASS"
  showEnum MODULE = "MODULE"
  showEnum CALL = "CALL"
  showEnum TYPE = "TYPE"
  showEnum INTERFACE = "INTERFACE"
  showEnum IMPLEMENTATION = "IMPLEMENTATION"
  showEnum (SyntaxType'Unrecognized (SyntaxType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "UNKNOWN_SYNTAX" = Prelude.Just UNKNOWN_SYNTAX
    | (Prelude.==) k "FUNCTION" = Prelude.Just FUNCTION
    | (Prelude.==) k "METHOD" = Prelude.Just METHOD
    | (Prelude.==) k "CLASS" = Prelude.Just CLASS
    | (Prelude.==) k "MODULE" = Prelude.Just MODULE
    | (Prelude.==) k "CALL" = Prelude.Just CALL
    | (Prelude.==) k "TYPE" = Prelude.Just TYPE
    | (Prelude.==) k "INTERFACE" = Prelude.Just INTERFACE
    | (Prelude.==) k "IMPLEMENTATION" = Prelude.Just IMPLEMENTATION
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded SyntaxType where
  minBound = UNKNOWN_SYNTAX
  maxBound = IMPLEMENTATION
instance Prelude.Enum SyntaxType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum SyntaxType: " (Prelude.show k__)))
        Prelude.id
        (Data.ProtoLens.maybeToEnum k__)
  fromEnum UNKNOWN_SYNTAX = 0
  fromEnum FUNCTION = 1
  fromEnum METHOD = 2
  fromEnum CLASS = 3
  fromEnum MODULE = 4
  fromEnum CALL = 5
  fromEnum TYPE = 6
  fromEnum INTERFACE = 7
  fromEnum IMPLEMENTATION = 8
  fromEnum (SyntaxType'Unrecognized (SyntaxType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ IMPLEMENTATION
    = Prelude.error
        "SyntaxType.succ: bad argument IMPLEMENTATION. This value would be out of bounds."
  succ UNKNOWN_SYNTAX = FUNCTION
  succ FUNCTION = METHOD
  succ METHOD = CLASS
  succ CLASS = MODULE
  succ MODULE = CALL
  succ CALL = TYPE
  succ TYPE = INTERFACE
  succ INTERFACE = IMPLEMENTATION
  succ (SyntaxType'Unrecognized _)
    = Prelude.error "SyntaxType.succ: bad argument: unrecognized value"
  pred UNKNOWN_SYNTAX
    = Prelude.error
        "SyntaxType.pred: bad argument UNKNOWN_SYNTAX. This value would be out of bounds."
  pred FUNCTION = UNKNOWN_SYNTAX
  pred METHOD = FUNCTION
  pred CLASS = METHOD
  pred MODULE = CLASS
  pred CALL = MODULE
  pred TYPE = CALL
  pred INTERFACE = TYPE
  pred IMPLEMENTATION = INTERFACE
  pred (SyntaxType'Unrecognized _)
    = Prelude.error "SyntaxType.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault SyntaxType where
  fieldDefault = UNKNOWN_SYNTAX
instance Control.DeepSeq.NFData SyntaxType where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Semantic_Fields.source' @:: Lens' TermEdge Data.Int.Int32@
         * 'Proto.Semantic_Fields.target' @:: Lens' TermEdge Data.Int.Int32@ -}
data TermEdge
  = TermEdge'_constructor {_TermEdge'source :: !Data.Int.Int32,
                           _TermEdge'target :: !Data.Int.Int32,
                           _TermEdge'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\bTermEdge\DC2\SYN\n\
      \\ACKsource\CAN\SOH \SOH(\ENQR\ACKsource\DC2\SYN\n\
      \\ACKtarget\CAN\STX \SOH(\ENQR\ACKtarget"
  packedFileDescriptor _ = packedFileDescriptor
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
  deriving stock (Prelude.Eq, Prelude.Ord)
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
  packedMessageDescriptor _
    = "\n\
      \\n\
      \TermVertex\DC2\ESC\n\
      \\tvertex_id\CAN\SOH \SOH(\ENQR\bvertexId\DC2\DC2\n\
      \\EOTterm\CAN\STX \SOH(\tR\EOTterm\DC2)\n\
      \\EOTspan\CAN\ETX \SOH(\v2\NAK.github.semantic.SpanR\EOTspan"
  packedFileDescriptor _ = packedFileDescriptor
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
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\SOsemantic.proto\DC2\SIgithub.semantic\"'\n\
    \\vPingRequest\DC2\CAN\n\
    \\aservice\CAN\SOH \SOH(\tR\aservice\"r\n\
    \\fPingResponse\DC2\SYN\n\
    \\ACKstatus\CAN\SOH \SOH(\tR\ACKstatus\DC2\SUB\n\
    \\bhostname\CAN\STX \SOH(\tR\bhostname\DC2\FS\n\
    \\ttimestamp\CAN\ETX \SOH(\tR\ttimestamp\DC2\DLE\n\
    \\ETXsha\CAN\EOT \SOH(\tR\ETXsha\"?\n\
    \\DLEParseTreeRequest\DC2+\n\
    \\ENQblobs\CAN\SOH \ETX(\v2\NAK.github.semantic.BlobR\ENQblobs\"F\n\
    \\ETBParseTreeSymbolResponse\DC2+\n\
    \\ENQfiles\CAN\SOH \ETX(\v2\NAK.github.semantic.FileR\ENQfiles\"S\n\
    \\SYNParseTreeGraphResponse\DC29\n\
    \\ENQfiles\CAN\SOH \ETX(\v2#.github.semantic.ParseTreeFileGraphR\ENQfiles\"@\n\
    \\DC1StackGraphRequest\DC2+\n\
    \\ENQblobs\CAN\SOH \ETX(\v2\NAK.github.semantic.BlobR\ENQblobs\"K\n\
    \\DC2StackGraphResponse\DC25\n\
    \\ENQfiles\CAN\SOH \ETX(\v2\US.github.semantic.StackGraphFileR\ENQfiles\"\227\SOH\n\
    \\DC2ParseTreeFileGraph\DC2\DC2\n\
    \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
    \\blanguage\CAN\STX \SOH(\tR\blanguage\DC27\n\
    \\bvertices\CAN\ETX \ETX(\v2\ESC.github.semantic.TermVertexR\bvertices\DC2/\n\
    \\ENQedges\CAN\EOT \ETX(\v2\EM.github.semantic.TermEdgeR\ENQedges\DC23\n\
    \\ACKerrors\CAN\ENQ \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors\":\n\
    \\bTermEdge\DC2\SYN\n\
    \\ACKsource\CAN\SOH \SOH(\ENQR\ACKsource\DC2\SYN\n\
    \\ACKtarget\CAN\STX \SOH(\ENQR\ACKtarget\"h\n\
    \\n\
    \TermVertex\DC2\ESC\n\
    \\tvertex_id\CAN\SOH \SOH(\ENQR\bvertexId\DC2\DC2\n\
    \\EOTterm\CAN\STX \SOH(\tR\EOTterm\DC2)\n\
    \\EOTspan\CAN\ETX \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\"\"\n\
    \\n\
    \ParseError\DC2\DC4\n\
    \\ENQerror\CAN\SOH \SOH(\tR\ENQerror\"Q\n\
    \\NAKDiffTreeGraphResponse\DC28\n\
    \\ENQfiles\CAN\SOH \ETX(\v2\".github.semantic.DiffTreeFileGraphR\ENQfiles\"\234\SOH\n\
    \\DC1DiffTreeFileGraph\DC2\DC2\n\
    \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
    \\blanguage\CAN\STX \SOH(\tR\blanguage\DC2;\n\
    \\bvertices\CAN\ETX \ETX(\v2\US.github.semantic.DiffTreeVertexR\bvertices\DC23\n\
    \\ENQedges\CAN\EOT \ETX(\v2\GS.github.semantic.DiffTreeEdgeR\ENQedges\DC23\n\
    \\ACKerrors\CAN\ENQ \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors\">\n\
    \\fDiffTreeEdge\DC2\SYN\n\
    \\ACKsource\CAN\SOH \SOH(\ENQR\ACKsource\DC2\SYN\n\
    \\ACKtarget\CAN\STX \SOH(\ENQR\ACKtarget\"\174\STX\n\
    \\SODiffTreeVertex\DC2$\n\
    \\SOdiff_vertex_id\CAN\SOH \SOH(\ENQR\fdiffVertexId\DC28\n\
    \\adeleted\CAN\STX \SOH(\v2\FS.github.semantic.DeletedTermH\NULR\adeleted\DC2;\n\
    \\binserted\CAN\ETX \SOH(\v2\GS.github.semantic.InsertedTermH\NULR\binserted\DC2;\n\
    \\breplaced\CAN\EOT \SOH(\v2\GS.github.semantic.ReplacedTermH\NULR\breplaced\DC25\n\
    \\ACKmerged\CAN\ENQ \SOH(\v2\ESC.github.semantic.MergedTermH\NULR\ACKmergedB\v\n\
    \\tdiff_term\"L\n\
    \\vDeletedTerm\DC2\DC2\n\
    \\EOTterm\CAN\SOH \SOH(\tR\EOTterm\DC2)\n\
    \\EOTspan\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\"M\n\
    \\fInsertedTerm\DC2\DC2\n\
    \\EOTterm\CAN\SOH \SOH(\tR\EOTterm\DC2)\n\
    \\EOTspan\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\"\188\SOH\n\
    \\fReplacedTerm\DC2\US\n\
    \\vbefore_term\CAN\SOH \SOH(\tR\n\
    \beforeTerm\DC26\n\
    \\vbefore_span\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\n\
    \beforeSpan\DC2\GS\n\
    \\n\
    \after_term\CAN\ETX \SOH(\tR\tafterTerm\DC24\n\
    \\n\
    \after_span\CAN\EOT \SOH(\v2\NAK.github.semantic.SpanR\tafterSpan\"\142\SOH\n\
    \\n\
    \MergedTerm\DC2\DC2\n\
    \\EOTterm\CAN\SOH \SOH(\tR\EOTterm\DC26\n\
    \\vbefore_span\CAN\STX \SOH(\v2\NAK.github.semantic.SpanR\n\
    \beforeSpan\DC24\n\
    \\n\
    \after_span\CAN\ETX \SOH(\v2\NAK.github.semantic.SpanR\tafterSpan\"P\n\
    \\EOTBlob\DC2\CAN\n\
    \\acontent\CAN\SOH \SOH(\tR\acontent\DC2\DC2\n\
    \\EOTpath\CAN\STX \SOH(\tR\EOTpath\DC2\SUB\n\
    \\blanguage\CAN\ETX \SOH(\tR\blanguage\"\185\SOH\n\
    \\EOTFile\DC2\DC2\n\
    \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
    \\blanguage\CAN\STX \SOH(\tR\blanguage\DC21\n\
    \\asymbols\CAN\ETX \ETX(\v2\ETB.github.semantic.SymbolR\asymbols\DC23\n\
    \\ACKerrors\CAN\EOT \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors\DC2\EM\n\
    \\bblob_oid\CAN\ENQ \SOH(\tR\ablobOid\"\153\STX\n\
    \\ACKSymbol\DC2\SYN\n\
    \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2\DC2\n\
    \\EOTkind\CAN\STX \SOH(\tR\EOTkind\DC2\DC2\n\
    \\EOTline\CAN\ETX \SOH(\tR\EOTline\DC2)\n\
    \\EOTspan\CAN\EOT \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\DC2.\n\
    \\EOTdocs\CAN\ENQ \SOH(\v2\SUB.github.semantic.DocstringR\EOTdocs\DC26\n\
    \\tnode_type\CAN\ACK \SOH(\SO2\EM.github.semantic.NodeTypeR\bnodeType\DC2<\n\
    \\vsyntax_type\CAN\a \SOH(\SO2\ESC.github.semantic.SyntaxTypeR\n\
    \syntaxType\")\n\
    \\tDocstring\DC2\FS\n\
    \\tdocstring\CAN\SOH \SOH(\tR\tdocstring\"6\n\
    \\bPosition\DC2\DC2\n\
    \\EOTline\CAN\SOH \SOH(\ENQR\EOTline\DC2\SYN\n\
    \\ACKcolumn\CAN\STX \SOH(\ENQR\ACKcolumn\"d\n\
    \\EOTSpan\DC2/\n\
    \\ENQstart\CAN\SOH \SOH(\v2\EM.github.semantic.PositionR\ENQstart\DC2+\n\
    \\ETXend\CAN\STX \SOH(\v2\EM.github.semantic.PositionR\ETXend\"\227\SOH\n\
    \\SOStackGraphFile\DC2\DC2\n\
    \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
    \\blanguage\CAN\STX \SOH(\tR\blanguage\DC25\n\
    \\ENQnodes\CAN\ETX \ETX(\v2\US.github.semantic.StackGraphNodeR\ENQnodes\DC25\n\
    \\ENQpaths\CAN\EOT \ETX(\v2\US.github.semantic.StackGraphPathR\ENQpaths\DC23\n\
    \\ACKerrors\CAN\ENQ \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors\"\233\SOH\n\
    \\SOStackGraphNode\DC2\SO\n\
    \\STXid\CAN\SOH \SOH(\ETXR\STXid\DC2\DC2\n\
    \\EOTname\CAN\STX \SOH(\tR\EOTname\DC2\DC2\n\
    \\EOTline\CAN\ETX \SOH(\tR\EOTline\DC2)\n\
    \\EOTspan\CAN\EOT \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\DC2<\n\
    \\vsyntax_type\CAN\ENQ \SOH(\SO2\ESC.github.semantic.SyntaxTypeR\n\
    \syntaxType\DC26\n\
    \\tnode_type\CAN\ACK \SOH(\SO2\EM.github.semantic.NodeTypeR\bnodeType\"\151\STX\n\
    \\SOStackGraphPath\DC22\n\
    \\NAKstarting_symbol_stack\CAN\SOH \ETX(\tR\DC3startingSymbolStack\DC29\n\
    \\EMstarting_scope_stack_size\CAN\STX \SOH(\ETXR\SYNstartingScopeStackSize\DC2\DC2\n\
    \\EOTfrom\CAN\ETX \SOH(\ETXR\EOTfrom\DC2\DC4\n\
    \\ENQedges\CAN\EOT \SOH(\tR\ENQedges\DC2\SO\n\
    \\STXto\CAN\ENQ \SOH(\ETXR\STXto\DC2,\n\
    \\DC2ending_scope_stack\CAN\ACK \ETX(\ETXR\DLEendingScopeStack\DC2.\n\
    \\DC3ending_symbol_stack\CAN\a \ETX(\tR\DC1endingSymbolStack*r\n\
    \\bNodeType\DC2\DLE\n\
    \\fUNKNOWN_NODE\DLE\NUL\DC2\SO\n\
    \\n\
    \ROOT_SCOPE\DLE\SOH\DC2\DC1\n\
    \\rJUMP_TO_SCOPE\DLE\STX\DC2\DC2\n\
    \\SOEXPORTED_SCOPE\DLE\ETX\DC2\SO\n\
    \\n\
    \DEFINITION\DLE\EOT\DC2\r\n\
    \\tREFERENCE\DLE\ENQ*\136\SOH\n\
    \\n\
    \SyntaxType\DC2\DC2\n\
    \\SOUNKNOWN_SYNTAX\DLE\NUL\DC2\f\n\
    \\bFUNCTION\DLE\SOH\DC2\n\
    \\n\
    \\ACKMETHOD\DLE\STX\DC2\t\n\
    \\ENQCLASS\DLE\ETX\DC2\n\
    \\n\
    \\ACKMODULE\DLE\EOT\DC2\b\n\
    \\EOTCALL\DLE\ENQ\DC2\b\n\
    \\EOTTYPE\DLE\ACK\DC2\r\n\
    \\tINTERFACE\DLE\a\DC2\DC2\n\
    \\SOIMPLEMENTATION\DLE\bB\DC2\234\STX\SISemantic::ProtoJ\200\&9\n\
    \\a\DC2\ENQ\NUL\NUL\194\SOH\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\CAN\n\
    \\b\n\
    \\SOH\b\DC2\ETX\EOT\NUL(\n\
    \\t\n\
    \\STX\b-\DC2\ETX\EOT\NUL(\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\ACK\NUL\b\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\ACK\b\DC3\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\a\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\EOT\a\STX\ACK\NAK\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\a\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\a\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\a\DC3\DC4\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\n\
    \\NUL\SI\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\n\
    \\b\DC4\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX\v\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\NUL\EOT\DC2\EOT\v\STX\n\
    \\SYN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\ETX\v\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX\v\t\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX\v\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX\f\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\SOH\EOT\DC2\EOT\f\STX\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\ETX\f\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX\f\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX\f\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX\r\STX\ETB\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\STX\EOT\DC2\EOT\r\STX\f\SYN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETX\r\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX\r\t\DC2\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX\r\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\SOH\STX\ETX\DC2\ETX\SO\STX\DC1\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\ETX\EOT\DC2\EOT\SO\STX\r\ETB\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ENQ\DC2\ETX\SO\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\SOH\DC2\ETX\SO\t\f\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\ETX\ETX\DC2\ETX\SO\SI\DLE\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\DC1\NUL\DC3\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\DC1\b\CAN\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DC2\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\EOT\DC2\ETX\DC2\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX\DC2\v\SI\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DC2\DLE\NAK\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DC2\CAN\EM\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT\NAK\NUL\ETB\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\NAK\b\US\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\SYN\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\EOT\DC2\ETX\SYN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ACK\DC2\ETX\SYN\v\SI\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\SYN\DLE\NAK\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\SYN\CAN\EM\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT\EM\NUL\ESC\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX\EM\b\RS\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX\SUB\STX(\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\EOT\DC2\ETX\SUB\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX\SUB\v\GS\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX\SUB\RS#\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX\SUB&'\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT\GS\NUL\US\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX\GS\b\EM\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX\RS\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\EOT\DC2\ETX\RS\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX\RS\v\SI\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX\RS\DLE\NAK\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX\RS\CAN\EM\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOT!\NUL#\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX!\b\SUB\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX\"\STX$\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\EOT\DC2\ETX\"\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ACK\DC2\ETX\"\v\EM\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX\"\SUB\US\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX\"\"#\n\
    \\n\
    \\n\
    \\STX\EOT\a\DC2\EOT%\NUL+\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX%\b\SUB\n\
    \\v\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETX&\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\EOT\DC2\EOT&\STX%\FS\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\ETX&\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETX&\t\r\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETX&\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETX'\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\EOT\DC2\EOT'\STX&\DC2\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\ETX'\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETX'\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETX'\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\a\STX\STX\DC2\ETX(\STX#\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\EOT\DC2\ETX(\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ACK\DC2\ETX(\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\ETX(\SYN\RS\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\ETX(!\"\n\
    \\v\n\
    \\EOT\EOT\a\STX\ETX\DC2\ETX)\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\a\STX\ETX\EOT\DC2\ETX)\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\ETX\ACK\DC2\ETX)\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\a\STX\ETX\SOH\DC2\ETX)\DC4\EM\n\
    \\f\n\
    \\ENQ\EOT\a\STX\ETX\ETX\DC2\ETX)\FS\GS\n\
    \\v\n\
    \\EOT\EOT\a\STX\EOT\DC2\ETX*\STX!\n\
    \\f\n\
    \\ENQ\EOT\a\STX\EOT\EOT\DC2\ETX*\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\a\STX\EOT\ACK\DC2\ETX*\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\a\STX\EOT\SOH\DC2\ETX*\SYN\FS\n\
    \\f\n\
    \\ENQ\EOT\a\STX\EOT\ETX\DC2\ETX*\US \n\
    \\n\
    \\n\
    \\STX\EOT\b\DC2\EOT-\NUL0\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETX-\b\DLE\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETX.\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\EOT\DC2\EOT.\STX-\DC2\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ENQ\DC2\ETX.\STX\a\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETX.\b\SO\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETX.\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\b\STX\SOH\DC2\ETX/\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\EOT\DC2\EOT/\STX.\DC3\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ENQ\DC2\ETX/\STX\a\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\ETX/\b\SO\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\ETX/\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\t\DC2\EOT2\NUL6\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETX2\b\DC2\n\
    \\v\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETX3\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\EOT\DC2\EOT3\STX2\DC4\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\ETX3\STX\a\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETX3\b\DC1\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETX3\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\t\STX\SOH\DC2\ETX4\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\EOT\DC2\EOT4\STX3\SYN\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\ETX4\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\ETX4\t\r\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\ETX4\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\t\STX\STX\DC2\ETX5\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\EOT\DC2\EOT5\STX4\DC2\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ACK\DC2\ETX5\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\ETX5\a\v\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\ETX5\SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\n\
    \\DC2\EOT8\NUL:\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETX8\b\DC2\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETX9\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\EOT\DC2\EOT9\STX8\DC4\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ENQ\DC2\ETX9\STX\b\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETX9\t\SO\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETX9\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\v\DC2\EOT<\NUL>\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETX<\b\GS\n\
    \\v\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETX=\STX'\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\EOT\DC2\ETX=\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ACK\DC2\ETX=\v\FS\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETX=\GS\"\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETX=%&\n\
    \\n\
    \\n\
    \\STX\EOT\f\DC2\EOT@\NULF\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETX@\b\EM\n\
    \\v\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXA\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\EOT\DC2\EOTA\STX@\ESC\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ENQ\DC2\ETXA\STX\b\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXA\t\r\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXA\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\f\STX\SOH\DC2\ETXB\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\f\STX\SOH\EOT\DC2\EOTB\STXA\DC2\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ENQ\DC2\ETXB\STX\b\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\SOH\DC2\ETXB\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ETX\DC2\ETXB\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\f\STX\STX\DC2\ETXC\STX'\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\EOT\DC2\ETXC\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\ACK\DC2\ETXC\v\EM\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\SOH\DC2\ETXC\SUB\"\n\
    \\f\n\
    \\ENQ\EOT\f\STX\STX\ETX\DC2\ETXC%&\n\
    \\v\n\
    \\EOT\EOT\f\STX\ETX\DC2\ETXD\STX\"\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\EOT\DC2\ETXD\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\ACK\DC2\ETXD\v\ETB\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\SOH\DC2\ETXD\CAN\GS\n\
    \\f\n\
    \\ENQ\EOT\f\STX\ETX\ETX\DC2\ETXD !\n\
    \\v\n\
    \\EOT\EOT\f\STX\EOT\DC2\ETXE\STX!\n\
    \\f\n\
    \\ENQ\EOT\f\STX\EOT\EOT\DC2\ETXE\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\EOT\ACK\DC2\ETXE\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\f\STX\EOT\SOH\DC2\ETXE\SYN\FS\n\
    \\f\n\
    \\ENQ\EOT\f\STX\EOT\ETX\DC2\ETXE\US \n\
    \\n\
    \\n\
    \\STX\EOT\r\DC2\EOTH\NULK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETXH\b\DC4\n\
    \\v\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETXI\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\r\STX\NUL\EOT\DC2\EOTI\STXH\SYN\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ENQ\DC2\ETXI\STX\a\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETXI\b\SO\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETXI\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\r\STX\SOH\DC2\ETXJ\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\r\STX\SOH\EOT\DC2\EOTJ\STXI\DC3\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ENQ\DC2\ETXJ\STX\a\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\SOH\DC2\ETXJ\b\SO\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ETX\DC2\ETXJ\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\SO\DC2\EOTM\NULU\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETXM\b\SYN\n\
    \\v\n\
    \\EOT\EOT\SO\STX\NUL\DC2\ETXN\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\SO\STX\NUL\EOT\DC2\EOTN\STXM\CAN\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ENQ\DC2\ETXN\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\ETXN\b\SYN\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\ETXN\EM\SUB\n\
    \\f\n\
    \\EOT\EOT\SO\b\NUL\DC2\EOTO\STXT\ETX\n\
    \\f\n\
    \\ENQ\EOT\SO\b\NUL\SOH\DC2\ETXO\b\DC1\n\
    \\v\n\
    \\EOT\EOT\SO\STX\SOH\DC2\ETXP\EOT\FS\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ACK\DC2\ETXP\EOT\SI\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\SOH\DC2\ETXP\DLE\ETB\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ETX\DC2\ETXP\SUB\ESC\n\
    \\v\n\
    \\EOT\EOT\SO\STX\STX\DC2\ETXQ\EOT\RS\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\ACK\DC2\ETXQ\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\SOH\DC2\ETXQ\DC1\EM\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\ETX\DC2\ETXQ\FS\GS\n\
    \\v\n\
    \\EOT\EOT\SO\STX\ETX\DC2\ETXR\EOT\RS\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\ACK\DC2\ETXR\EOT\DLE\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\SOH\DC2\ETXR\DC1\EM\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\ETX\DC2\ETXR\FS\GS\n\
    \\v\n\
    \\EOT\EOT\SO\STX\EOT\DC2\ETXS\EOT\SUB\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\ACK\DC2\ETXS\EOT\SO\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\SOH\DC2\ETXS\SI\NAK\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\ETX\DC2\ETXS\CAN\EM\n\
    \\n\
    \\n\
    \\STX\EOT\SI\DC2\EOTW\NULZ\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SI\SOH\DC2\ETXW\b\DC3\n\
    \\v\n\
    \\EOT\EOT\SI\STX\NUL\DC2\ETXX\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\NUL\EOT\DC2\EOTX\STXW\NAK\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ENQ\DC2\ETXX\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\SOH\DC2\ETXX\t\r\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ETX\DC2\ETXX\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\SI\STX\SOH\DC2\ETXY\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\SOH\EOT\DC2\EOTY\STXX\DC2\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ACK\DC2\ETXY\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\SOH\DC2\ETXY\a\v\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ETX\DC2\ETXY\SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\DLE\DC2\EOT\\\NUL_\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\DLE\SOH\DC2\ETX\\\b\DC4\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\NUL\DC2\ETX]\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\NUL\EOT\DC2\EOT]\STX\\\SYN\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\ENQ\DC2\ETX]\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\SOH\DC2\ETX]\t\r\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\ETX\DC2\ETX]\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\SOH\DC2\ETX^\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\EOT\DC2\EOT^\STX]\DC2\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\ACK\DC2\ETX^\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\SOH\DC2\ETX^\a\v\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\ETX\DC2\ETX^\SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\DC1\DC2\EOTa\NULf\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\DC1\SOH\DC2\ETXa\b\DC4\n\
    \\v\n\
    \\EOT\EOT\DC1\STX\NUL\DC2\ETXb\STX\EM\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\NUL\EOT\DC2\EOTb\STXa\SYN\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\NUL\ENQ\DC2\ETXb\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\NUL\SOH\DC2\ETXb\t\DC4\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\NUL\ETX\DC2\ETXb\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\DC1\STX\SOH\DC2\ETXc\STX\ETB\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\SOH\EOT\DC2\EOTc\STXb\EM\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\SOH\ACK\DC2\ETXc\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\SOH\SOH\DC2\ETXc\a\DC2\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\SOH\ETX\DC2\ETXc\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\DC1\STX\STX\DC2\ETXd\STX\CAN\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\STX\EOT\DC2\EOTd\STXc\ETB\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\STX\ENQ\DC2\ETXd\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\STX\SOH\DC2\ETXd\t\DC3\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\STX\ETX\DC2\ETXd\SYN\ETB\n\
    \\v\n\
    \\EOT\EOT\DC1\STX\ETX\DC2\ETXe\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC1\STX\ETX\EOT\DC2\EOTe\STXd\CAN\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\ETX\ACK\DC2\ETXe\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\ETX\SOH\DC2\ETXe\a\DC1\n\
    \\f\n\
    \\ENQ\EOT\DC1\STX\ETX\ETX\DC2\ETXe\DC4\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\DC2\DC2\EOTh\NULl\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\DC2\SOH\DC2\ETXh\b\DC2\n\
    \\v\n\
    \\EOT\EOT\DC2\STX\NUL\DC2\ETXi\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\NUL\EOT\DC2\EOTi\STXh\DC4\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\NUL\ENQ\DC2\ETXi\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\NUL\SOH\DC2\ETXi\t\r\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\NUL\ETX\DC2\ETXi\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\DC2\STX\SOH\DC2\ETXj\STX\ETB\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\SOH\EOT\DC2\EOTj\STXi\DC2\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\SOH\ACK\DC2\ETXj\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\SOH\SOH\DC2\ETXj\a\DC2\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\SOH\ETX\DC2\ETXj\NAK\SYN\n\
    \\v\n\
    \\EOT\EOT\DC2\STX\STX\DC2\ETXk\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC2\STX\STX\EOT\DC2\EOTk\STXj\ETB\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\STX\ACK\DC2\ETXk\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\STX\SOH\DC2\ETXk\a\DC1\n\
    \\f\n\
    \\ENQ\EOT\DC2\STX\STX\ETX\DC2\ETXk\DC4\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\DC3\DC2\EOTn\NULr\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\DC3\SOH\DC2\ETXn\b\f\n\
    \\v\n\
    \\EOT\EOT\DC3\STX\NUL\DC2\ETXo\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\NUL\EOT\DC2\EOTo\STXn\SO\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\NUL\ENQ\DC2\ETXo\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\NUL\SOH\DC2\ETXo\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\NUL\ETX\DC2\ETXo\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\DC3\STX\SOH\DC2\ETXp\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\SOH\EOT\DC2\EOTp\STXo\NAK\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\SOH\ENQ\DC2\ETXp\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\SOH\SOH\DC2\ETXp\t\r\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\SOH\ETX\DC2\ETXp\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\DC3\STX\STX\DC2\ETXq\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC3\STX\STX\EOT\DC2\EOTq\STXp\DC2\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\STX\ENQ\DC2\ETXq\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\STX\SOH\DC2\ETXq\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\DC3\STX\STX\ETX\DC2\ETXq\DC4\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\DC4\DC2\EOTt\NULz\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\DC4\SOH\DC2\ETXt\b\f\n\
    \\v\n\
    \\EOT\EOT\DC4\STX\NUL\DC2\ETXu\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\NUL\EOT\DC2\EOTu\STXt\SO\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\NUL\ENQ\DC2\ETXu\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\NUL\SOH\DC2\ETXu\t\r\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\NUL\ETX\DC2\ETXu\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\DC4\STX\SOH\DC2\ETXv\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\SOH\EOT\DC2\EOTv\STXu\DC2\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\SOH\ENQ\DC2\ETXv\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\SOH\SOH\DC2\ETXv\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\SOH\ETX\DC2\ETXv\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\DC4\STX\STX\DC2\ETXw\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\STX\EOT\DC2\ETXw\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\STX\ACK\DC2\ETXw\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\STX\SOH\DC2\ETXw\DC2\EM\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\STX\ETX\DC2\ETXw\FS\GS\n\
    \\v\n\
    \\EOT\EOT\DC4\STX\ETX\DC2\ETXx\STX!\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\ETX\EOT\DC2\ETXx\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\ETX\ACK\DC2\ETXx\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\ETX\SOH\DC2\ETXx\SYN\FS\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\ETX\ETX\DC2\ETXx\US \n\
    \\v\n\
    \\EOT\EOT\DC4\STX\EOT\DC2\ETXy\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\DC4\STX\EOT\EOT\DC2\EOTy\STXx!\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\EOT\ENQ\DC2\ETXy\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\EOT\SOH\DC2\ETXy\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\DC4\STX\EOT\ETX\DC2\ETXy\DC4\NAK\n\
    \\v\n\
    \\STX\EOT\NAK\DC2\ENQ|\NUL\132\SOH\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NAK\SOH\DC2\ETX|\b\SO\n\
    \\v\n\
    \\EOT\EOT\NAK\STX\NUL\DC2\ETX}\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\NUL\EOT\DC2\EOT}\STX|\DLE\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\NUL\ENQ\DC2\ETX}\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\NUL\SOH\DC2\ETX}\t\SI\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\NUL\ETX\DC2\ETX}\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\NAK\STX\SOH\DC2\ETX~\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\SOH\EOT\DC2\EOT~\STX}\DC4\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\SOH\ENQ\DC2\ETX~\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\SOH\SOH\DC2\ETX~\t\r\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\SOH\ETX\DC2\ETX~\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\NAK\STX\STX\DC2\ETX\DEL\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\STX\EOT\DC2\EOT\DEL\STX~\DC2\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\STX\ENQ\DC2\ETX\DEL\STX\b\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\STX\SOH\DC2\ETX\DEL\t\r\n\
    \\f\n\
    \\ENQ\EOT\NAK\STX\STX\ETX\DC2\ETX\DEL\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\ETX\DC2\EOT\128\SOH\STX\DLE\n\
    \\SO\n\
    \\ENQ\EOT\NAK\STX\ETX\EOT\DC2\ENQ\128\SOH\STX\DEL\DC2\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ETX\ACK\DC2\EOT\128\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ETX\SOH\DC2\EOT\128\SOH\a\v\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ETX\ETX\DC2\EOT\128\SOH\SO\SI\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\EOT\DC2\EOT\129\SOH\STX\NAK\n\
    \\SI\n\
    \\ENQ\EOT\NAK\STX\EOT\EOT\DC2\ACK\129\SOH\STX\128\SOH\DLE\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\EOT\ACK\DC2\EOT\129\SOH\STX\v\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\EOT\SOH\DC2\EOT\129\SOH\f\DLE\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\EOT\ETX\DC2\EOT\129\SOH\DC3\DC4\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\ENQ\DC2\EOT\130\SOH\STX\EM\n\
    \\SI\n\
    \\ENQ\EOT\NAK\STX\ENQ\EOT\DC2\ACK\130\SOH\STX\129\SOH\NAK\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ENQ\ACK\DC2\EOT\130\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ENQ\SOH\DC2\EOT\130\SOH\v\DC4\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ENQ\ETX\DC2\EOT\130\SOH\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\NAK\STX\ACK\DC2\EOT\131\SOH\STX\GS\n\
    \\SI\n\
    \\ENQ\EOT\NAK\STX\ACK\EOT\DC2\ACK\131\SOH\STX\130\SOH\EM\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ACK\ACK\DC2\EOT\131\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ACK\SOH\DC2\EOT\131\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\NAK\STX\ACK\ETX\DC2\EOT\131\SOH\ESC\FS\n\
    \\f\n\
    \\STX\EOT\SYN\DC2\ACK\134\SOH\NUL\136\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SYN\SOH\DC2\EOT\134\SOH\b\DC1\n\
    \\f\n\
    \\EOT\EOT\SYN\STX\NUL\DC2\EOT\135\SOH\STX\ETB\n\
    \\SI\n\
    \\ENQ\EOT\SYN\STX\NUL\EOT\DC2\ACK\135\SOH\STX\134\SOH\DC3\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ENQ\DC2\EOT\135\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\SOH\DC2\EOT\135\SOH\t\DC2\n\
    \\r\n\
    \\ENQ\EOT\SYN\STX\NUL\ETX\DC2\EOT\135\SOH\NAK\SYN\n\
    \\f\n\
    \\STX\EOT\ETB\DC2\ACK\138\SOH\NUL\141\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\ETB\SOH\DC2\EOT\138\SOH\b\DLE\n\
    \\f\n\
    \\EOT\EOT\ETB\STX\NUL\DC2\EOT\139\SOH\STX\DC1\n\
    \\SI\n\
    \\ENQ\EOT\ETB\STX\NUL\EOT\DC2\ACK\139\SOH\STX\138\SOH\DC2\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\NUL\ENQ\DC2\EOT\139\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\NUL\SOH\DC2\EOT\139\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\NUL\ETX\DC2\EOT\139\SOH\SI\DLE\n\
    \\f\n\
    \\EOT\EOT\ETB\STX\SOH\DC2\EOT\140\SOH\STX\DC3\n\
    \\SI\n\
    \\ENQ\EOT\ETB\STX\SOH\EOT\DC2\ACK\140\SOH\STX\139\SOH\DC1\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\SOH\ENQ\DC2\EOT\140\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\SOH\SOH\DC2\EOT\140\SOH\b\SO\n\
    \\r\n\
    \\ENQ\EOT\ETB\STX\SOH\ETX\DC2\EOT\140\SOH\DC1\DC2\n\
    \\f\n\
    \\STX\EOT\CAN\DC2\ACK\143\SOH\NUL\146\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\CAN\SOH\DC2\EOT\143\SOH\b\f\n\
    \\f\n\
    \\EOT\EOT\CAN\STX\NUL\DC2\EOT\144\SOH\STX\NAK\n\
    \\SI\n\
    \\ENQ\EOT\CAN\STX\NUL\EOT\DC2\ACK\144\SOH\STX\143\SOH\SO\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\NUL\ACK\DC2\EOT\144\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\NUL\SOH\DC2\EOT\144\SOH\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\NUL\ETX\DC2\EOT\144\SOH\DC3\DC4\n\
    \\f\n\
    \\EOT\EOT\CAN\STX\SOH\DC2\EOT\145\SOH\STX\DC3\n\
    \\SI\n\
    \\ENQ\EOT\CAN\STX\SOH\EOT\DC2\ACK\145\SOH\STX\144\SOH\NAK\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\SOH\ACK\DC2\EOT\145\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\SOH\SOH\DC2\EOT\145\SOH\v\SO\n\
    \\r\n\
    \\ENQ\EOT\CAN\STX\SOH\ETX\DC2\EOT\145\SOH\DC1\DC2\n\
    \\f\n\
    \\STX\EOT\EM\DC2\ACK\148\SOH\NUL\154\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\EM\SOH\DC2\EOT\148\SOH\b\SYN\n\
    \\f\n\
    \\EOT\EOT\EM\STX\NUL\DC2\EOT\149\SOH\STX\DC2\n\
    \\SI\n\
    \\ENQ\EOT\EM\STX\NUL\EOT\DC2\ACK\149\SOH\STX\148\SOH\CAN\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\NUL\ENQ\DC2\EOT\149\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\NUL\SOH\DC2\EOT\149\SOH\t\r\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\NUL\ETX\DC2\EOT\149\SOH\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\EM\STX\SOH\DC2\EOT\150\SOH\STX\SYN\n\
    \\SI\n\
    \\ENQ\EOT\EM\STX\SOH\EOT\DC2\ACK\150\SOH\STX\149\SOH\DC2\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\SOH\ENQ\DC2\EOT\150\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\SOH\SOH\DC2\EOT\150\SOH\t\DC1\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\SOH\ETX\DC2\EOT\150\SOH\DC4\NAK\n\
    \\f\n\
    \\EOT\EOT\EM\STX\STX\DC2\EOT\151\SOH\STX$\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\EOT\DC2\EOT\151\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\ACK\DC2\EOT\151\SOH\v\EM\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\SOH\DC2\EOT\151\SOH\SUB\US\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\STX\ETX\DC2\EOT\151\SOH\"#\n\
    \\f\n\
    \\EOT\EOT\EM\STX\ETX\DC2\EOT\152\SOH\STX$\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\ETX\EOT\DC2\EOT\152\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\ETX\ACK\DC2\EOT\152\SOH\v\EM\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\ETX\SOH\DC2\EOT\152\SOH\SUB\US\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\ETX\ETX\DC2\EOT\152\SOH\"#\n\
    \\f\n\
    \\EOT\EOT\EM\STX\EOT\DC2\EOT\153\SOH\STX!\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\EOT\EOT\DC2\EOT\153\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\EOT\ACK\DC2\EOT\153\SOH\v\NAK\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\EOT\SOH\DC2\EOT\153\SOH\SYN\FS\n\
    \\r\n\
    \\ENQ\EOT\EM\STX\EOT\ETX\DC2\EOT\153\SOH\US \n\
    \\f\n\
    \\STX\ENQ\NUL\DC2\ACK\156\SOH\NUL\163\SOH\SOH\n\
    \\v\n\
    \\ETX\ENQ\NUL\SOH\DC2\EOT\156\SOH\ENQ\r\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\EOT\157\SOH\STX\DC3\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\EOT\157\SOH\STX\SO\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\EOT\157\SOH\DC1\DC2\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\EOT\158\SOH\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\EOT\158\SOH\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\EOT\158\SOH\SI\DLE\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\EOT\159\SOH\STX\DC4\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\EOT\159\SOH\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\EOT\159\SOH\DC2\DC3\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\ETX\DC2\EOT\160\SOH\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\ETX\SOH\DC2\EOT\160\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\ETX\STX\DC2\EOT\160\SOH\DC3\DC4\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\EOT\DC2\EOT\161\SOH\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\EOT\SOH\DC2\EOT\161\SOH\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\EOT\STX\DC2\EOT\161\SOH\SI\DLE\n\
    \\f\n\
    \\EOT\ENQ\NUL\STX\ENQ\DC2\EOT\162\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\ENQ\SOH\DC2\EOT\162\SOH\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\ENQ\STX\DC2\EOT\162\SOH\SO\SI\n\
    \\f\n\
    \\STX\ENQ\SOH\DC2\ACK\165\SOH\NUL\175\SOH\SOH\n\
    \\v\n\
    \\ETX\ENQ\SOH\SOH\DC2\EOT\165\SOH\ENQ\SI\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\NUL\DC2\EOT\166\SOH\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\NUL\SOH\DC2\EOT\166\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\NUL\STX\DC2\EOT\166\SOH\DC3\DC4\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\SOH\DC2\EOT\167\SOH\STX\SI\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\SOH\SOH\DC2\EOT\167\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\SOH\STX\DC2\EOT\167\SOH\r\SO\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\STX\DC2\EOT\168\SOH\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\STX\SOH\DC2\EOT\168\SOH\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\STX\STX\DC2\EOT\168\SOH\v\f\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\ETX\DC2\EOT\169\SOH\STX\f\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\ETX\SOH\DC2\EOT\169\SOH\STX\a\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\ETX\STX\DC2\EOT\169\SOH\n\
    \\v\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\EOT\DC2\EOT\170\SOH\STX\r\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\EOT\SOH\DC2\EOT\170\SOH\STX\b\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\EOT\STX\DC2\EOT\170\SOH\v\f\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\ENQ\DC2\EOT\171\SOH\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\ENQ\SOH\DC2\EOT\171\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\ENQ\STX\DC2\EOT\171\SOH\t\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\ACK\DC2\EOT\172\SOH\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\ACK\SOH\DC2\EOT\172\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\ACK\STX\DC2\EOT\172\SOH\t\n\
    \\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\a\DC2\EOT\173\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\a\SOH\DC2\EOT\173\SOH\STX\v\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\a\STX\DC2\EOT\173\SOH\SO\SI\n\
    \\f\n\
    \\EOT\ENQ\SOH\STX\b\DC2\EOT\174\SOH\STX\NAK\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\b\SOH\DC2\EOT\174\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\b\STX\DC2\EOT\174\SOH\DC3\DC4\n\
    \\f\n\
    \\STX\EOT\SUB\DC2\ACK\177\SOH\NUL\184\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\SUB\SOH\DC2\EOT\177\SOH\b\SYN\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\NUL\DC2\EOT\178\SOH\STX\SI\n\
    \\SI\n\
    \\ENQ\EOT\SUB\STX\NUL\EOT\DC2\ACK\178\SOH\STX\177\SOH\CAN\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\NUL\ENQ\DC2\EOT\178\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\NUL\SOH\DC2\EOT\178\SOH\b\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\NUL\ETX\DC2\EOT\178\SOH\r\SO\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\SOH\DC2\EOT\179\SOH\STX\DC2\n\
    \\SI\n\
    \\ENQ\EOT\SUB\STX\SOH\EOT\DC2\ACK\179\SOH\STX\178\SOH\SI\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\SOH\ENQ\DC2\EOT\179\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\SOH\SOH\DC2\EOT\179\SOH\t\r\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\SOH\ETX\DC2\EOT\179\SOH\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\STX\DC2\EOT\180\SOH\STX\DC2\n\
    \\SI\n\
    \\ENQ\EOT\SUB\STX\STX\EOT\DC2\ACK\180\SOH\STX\179\SOH\DC2\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\STX\ENQ\DC2\EOT\180\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\STX\SOH\DC2\EOT\180\SOH\t\r\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\STX\ETX\DC2\EOT\180\SOH\DLE\DC1\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\ETX\DC2\EOT\181\SOH\STX\DLE\n\
    \\SI\n\
    \\ENQ\EOT\SUB\STX\ETX\EOT\DC2\ACK\181\SOH\STX\180\SOH\DC2\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\ETX\ACK\DC2\EOT\181\SOH\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\ETX\SOH\DC2\EOT\181\SOH\a\v\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\ETX\ETX\DC2\EOT\181\SOH\SO\SI\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\EOT\DC2\EOT\182\SOH\STX\GS\n\
    \\SI\n\
    \\ENQ\EOT\SUB\STX\EOT\EOT\DC2\ACK\182\SOH\STX\181\SOH\DLE\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\EOT\ACK\DC2\EOT\182\SOH\STX\f\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\EOT\SOH\DC2\EOT\182\SOH\r\CAN\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\EOT\ETX\DC2\EOT\182\SOH\ESC\FS\n\
    \\f\n\
    \\EOT\EOT\SUB\STX\ENQ\DC2\EOT\183\SOH\STX\EM\n\
    \\SI\n\
    \\ENQ\EOT\SUB\STX\ENQ\EOT\DC2\ACK\183\SOH\STX\182\SOH\GS\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\ENQ\ACK\DC2\EOT\183\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\ENQ\SOH\DC2\EOT\183\SOH\v\DC4\n\
    \\r\n\
    \\ENQ\EOT\SUB\STX\ENQ\ETX\DC2\EOT\183\SOH\ETB\CAN\n\
    \\f\n\
    \\STX\EOT\ESC\DC2\ACK\186\SOH\NUL\194\SOH\SOH\n\
    \\v\n\
    \\ETX\EOT\ESC\SOH\DC2\EOT\186\SOH\b\SYN\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\NUL\DC2\EOT\187\SOH\STX,\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\NUL\EOT\DC2\EOT\187\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\NUL\ENQ\DC2\EOT\187\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\NUL\SOH\DC2\EOT\187\SOH\DC2'\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\NUL\ETX\DC2\EOT\187\SOH*+\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\SOH\DC2\EOT\188\SOH\STX&\n\
    \\SI\n\
    \\ENQ\EOT\ESC\STX\SOH\EOT\DC2\ACK\188\SOH\STX\187\SOH,\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\SOH\ENQ\DC2\EOT\188\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\SOH\SOH\DC2\EOT\188\SOH\b!\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\SOH\ETX\DC2\EOT\188\SOH$%\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\STX\DC2\EOT\189\SOH\STX\DC1\n\
    \\SI\n\
    \\ENQ\EOT\ESC\STX\STX\EOT\DC2\ACK\189\SOH\STX\188\SOH&\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\STX\ENQ\DC2\EOT\189\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\STX\SOH\DC2\EOT\189\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\STX\ETX\DC2\EOT\189\SOH\SI\DLE\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\ETX\DC2\EOT\190\SOH\STX\DC3\n\
    \\SI\n\
    \\ENQ\EOT\ESC\STX\ETX\EOT\DC2\ACK\190\SOH\STX\189\SOH\DC1\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ETX\ENQ\DC2\EOT\190\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ETX\SOH\DC2\EOT\190\SOH\t\SO\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ETX\ETX\DC2\EOT\190\SOH\DC1\DC2\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\EOT\DC2\EOT\191\SOH\STX\SI\n\
    \\SI\n\
    \\ENQ\EOT\ESC\STX\EOT\EOT\DC2\ACK\191\SOH\STX\190\SOH\DC3\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\EOT\ENQ\DC2\EOT\191\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\EOT\SOH\DC2\EOT\191\SOH\b\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\EOT\ETX\DC2\EOT\191\SOH\r\SO\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\ENQ\DC2\EOT\192\SOH\STX(\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ENQ\EOT\DC2\EOT\192\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ENQ\ENQ\DC2\EOT\192\SOH\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ENQ\SOH\DC2\EOT\192\SOH\DC1#\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ENQ\ETX\DC2\EOT\192\SOH&'\n\
    \\f\n\
    \\EOT\EOT\ESC\STX\ACK\DC2\EOT\193\SOH\STX*\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ACK\EOT\DC2\EOT\193\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ACK\ENQ\DC2\EOT\193\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ACK\SOH\DC2\EOT\193\SOH\DC2%\n\
    \\r\n\
    \\ENQ\EOT\ESC\STX\ACK\ETX\DC2\EOT\193\SOH()b\ACKproto3"