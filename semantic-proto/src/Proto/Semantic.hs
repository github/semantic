{- This file was auto-generated from semantic.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Semantic (
        Blob(), ByteRange(), Docstring(), File(), NodeType(..), NodeType(),
        NodeType'UnrecognizedValue, ParseError(), ParseTreeRequest(),
        ParseTreeSymbolResponse(), PingRequest(), PingResponse(),
        Position(), Span(), StackGraphFile(), StackGraphNode(),
        StackGraphPath(), StackGraphRequest(), StackGraphResponse(),
        Symbol(), SyntaxType(..), SyntaxType(),
        SyntaxType'UnrecognizedValue
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
     
         * 'Proto.Semantic_Fields.start' @:: Lens' ByteRange Data.Int.Int32@
         * 'Proto.Semantic_Fields.end' @:: Lens' ByteRange Data.Int.Int32@ -}
data ByteRange
  = ByteRange'_constructor {_ByteRange'start :: !Data.Int.Int32,
                            _ByteRange'end :: !Data.Int.Int32,
                            _ByteRange'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ByteRange where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ByteRange "start" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ByteRange'start (\ x__ y__ -> x__ {_ByteRange'start = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ByteRange "end" Data.Int.Int32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ByteRange'end (\ x__ y__ -> x__ {_ByteRange'end = y__}))
        Prelude.id
instance Data.ProtoLens.Message ByteRange where
  messageName _ = Data.Text.pack "github.semantic.ByteRange"
  packedMessageDescriptor _
    = "\n\
      \\tByteRange\DC2\DC4\n\
      \\ENQstart\CAN\SOH \SOH(\ENQR\ENQstart\DC2\DLE\n\
      \\ETXend\CAN\STX \SOH(\ENQR\ETXend"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        start__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "start"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"start")) ::
              Data.ProtoLens.FieldDescriptor ByteRange
        end__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "end"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"end")) ::
              Data.ProtoLens.FieldDescriptor ByteRange
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, start__field_descriptor),
           (Data.ProtoLens.Tag 2, end__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ByteRange'_unknownFields
        (\ x__ y__ -> x__ {_ByteRange'_unknownFields = y__})
  defMessage
    = ByteRange'_constructor
        {_ByteRange'start = Data.ProtoLens.fieldDefault,
         _ByteRange'end = Data.ProtoLens.fieldDefault,
         _ByteRange'_unknownFields = []}
  parseMessage
    = let
        loop :: ByteRange -> Data.ProtoLens.Encoding.Bytes.Parser ByteRange
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
                                       "start"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"start") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
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
          (do loop Data.ProtoLens.defMessage) "ByteRange"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"start") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"end") _x
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
instance Control.DeepSeq.NFData ByteRange where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ByteRange'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ByteRange'start x__)
                (Control.DeepSeq.deepseq (_ByteRange'end x__) ()))
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
newtype NodeType'UnrecognizedValue
  = NodeType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data NodeType
  = ROOT_SCOPE |
    JUMP_TO_SCOPE |
    EXPORTED_SCOPE |
    DEFINITION |
    REFERENCE |
    NodeType'Unrecognized !NodeType'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum NodeType where
  maybeToEnum 0 = Prelude.Just ROOT_SCOPE
  maybeToEnum 1 = Prelude.Just JUMP_TO_SCOPE
  maybeToEnum 2 = Prelude.Just EXPORTED_SCOPE
  maybeToEnum 3 = Prelude.Just DEFINITION
  maybeToEnum 4 = Prelude.Just REFERENCE
  maybeToEnum k
    = Prelude.Just
        (NodeType'Unrecognized
           (NodeType'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum ROOT_SCOPE = "ROOT_SCOPE"
  showEnum JUMP_TO_SCOPE = "JUMP_TO_SCOPE"
  showEnum EXPORTED_SCOPE = "EXPORTED_SCOPE"
  showEnum DEFINITION = "DEFINITION"
  showEnum REFERENCE = "REFERENCE"
  showEnum (NodeType'Unrecognized (NodeType'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "ROOT_SCOPE" = Prelude.Just ROOT_SCOPE
    | (Prelude.==) k "JUMP_TO_SCOPE" = Prelude.Just JUMP_TO_SCOPE
    | (Prelude.==) k "EXPORTED_SCOPE" = Prelude.Just EXPORTED_SCOPE
    | (Prelude.==) k "DEFINITION" = Prelude.Just DEFINITION
    | (Prelude.==) k "REFERENCE" = Prelude.Just REFERENCE
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded NodeType where
  minBound = ROOT_SCOPE
  maxBound = REFERENCE
instance Prelude.Enum NodeType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum NodeType: " (Prelude.show k__)))
        Prelude.id
        (Data.ProtoLens.maybeToEnum k__)
  fromEnum ROOT_SCOPE = 0
  fromEnum JUMP_TO_SCOPE = 1
  fromEnum EXPORTED_SCOPE = 2
  fromEnum DEFINITION = 3
  fromEnum REFERENCE = 4
  fromEnum (NodeType'Unrecognized (NodeType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ REFERENCE
    = Prelude.error
        "NodeType.succ: bad argument REFERENCE. This value would be out of bounds."
  succ ROOT_SCOPE = JUMP_TO_SCOPE
  succ JUMP_TO_SCOPE = EXPORTED_SCOPE
  succ EXPORTED_SCOPE = DEFINITION
  succ DEFINITION = REFERENCE
  succ (NodeType'Unrecognized _)
    = Prelude.error "NodeType.succ: bad argument: unrecognized value"
  pred ROOT_SCOPE
    = Prelude.error
        "NodeType.pred: bad argument ROOT_SCOPE. This value would be out of bounds."
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
  fieldDefault = ROOT_SCOPE
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
         * 'Proto.Semantic_Fields.syntaxType' @:: Lens' Symbol SyntaxType@
         * 'Proto.Semantic_Fields.utf16CodeUnitSpan' @:: Lens' Symbol Span@
         * 'Proto.Semantic_Fields.maybe'utf16CodeUnitSpan' @:: Lens' Symbol (Prelude.Maybe Span)@
         * 'Proto.Semantic_Fields.byteRange' @:: Lens' Symbol ByteRange@
         * 'Proto.Semantic_Fields.maybe'byteRange' @:: Lens' Symbol (Prelude.Maybe ByteRange)@ -}
data Symbol
  = Symbol'_constructor {_Symbol'symbol :: !Data.Text.Text,
                         _Symbol'kind :: !Data.Text.Text,
                         _Symbol'line :: !Data.Text.Text,
                         _Symbol'span :: !(Prelude.Maybe Span),
                         _Symbol'docs :: !(Prelude.Maybe Docstring),
                         _Symbol'nodeType :: !NodeType,
                         _Symbol'syntaxType :: !SyntaxType,
                         _Symbol'utf16CodeUnitSpan :: !(Prelude.Maybe Span),
                         _Symbol'byteRange :: !(Prelude.Maybe ByteRange),
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
instance Data.ProtoLens.Field.HasField Symbol "utf16CodeUnitSpan" Span where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'utf16CodeUnitSpan
           (\ x__ y__ -> x__ {_Symbol'utf16CodeUnitSpan = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Symbol "maybe'utf16CodeUnitSpan" (Prelude.Maybe Span) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'utf16CodeUnitSpan
           (\ x__ y__ -> x__ {_Symbol'utf16CodeUnitSpan = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Symbol "byteRange" ByteRange where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'byteRange (\ x__ y__ -> x__ {_Symbol'byteRange = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Symbol "maybe'byteRange" (Prelude.Maybe ByteRange) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Symbol'byteRange (\ x__ y__ -> x__ {_Symbol'byteRange = y__}))
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
      \syntaxType\DC2F\n\
      \\DC4utf16_code_unit_span\CAN\b \SOH(\v2\NAK.github.semantic.SpanR\DC1utf16CodeUnitSpan\DC29\n\
      \\n\
      \byte_range\CAN\t \SOH(\v2\SUB.github.semantic.ByteRangeR\tbyteRange"
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
        utf16CodeUnitSpan__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "utf16_code_unit_span"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Span)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'utf16CodeUnitSpan")) ::
              Data.ProtoLens.FieldDescriptor Symbol
        byteRange__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "byte_range"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ByteRange)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'byteRange")) ::
              Data.ProtoLens.FieldDescriptor Symbol
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, symbol__field_descriptor),
           (Data.ProtoLens.Tag 2, kind__field_descriptor),
           (Data.ProtoLens.Tag 3, line__field_descriptor),
           (Data.ProtoLens.Tag 4, span__field_descriptor),
           (Data.ProtoLens.Tag 5, docs__field_descriptor),
           (Data.ProtoLens.Tag 6, nodeType__field_descriptor),
           (Data.ProtoLens.Tag 7, syntaxType__field_descriptor),
           (Data.ProtoLens.Tag 8, utf16CodeUnitSpan__field_descriptor),
           (Data.ProtoLens.Tag 9, byteRange__field_descriptor)]
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
         _Symbol'utf16CodeUnitSpan = Prelude.Nothing,
         _Symbol'byteRange = Prelude.Nothing, _Symbol'_unknownFields = []}
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
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "utf16_code_unit_span"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"utf16CodeUnitSpan") y x)
                        74
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "byte_range"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"byteRange") y x)
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
                               ((Data.Monoid.<>)
                                  (case
                                       Lens.Family2.view
                                         (Data.ProtoLens.Field.field @"maybe'utf16CodeUnitSpan") _x
                                   of
                                     Prelude.Nothing -> Data.Monoid.mempty
                                     (Prelude.Just _v)
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                            ((Prelude..)
                                               (\ bs
                                                  -> (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                               Data.ProtoLens.encodeMessage
                                               _v))
                                  ((Data.Monoid.<>)
                                     (case
                                          Lens.Family2.view
                                            (Data.ProtoLens.Field.field @"maybe'byteRange") _x
                                      of
                                        Prelude.Nothing -> Data.Monoid.mempty
                                        (Prelude.Just _v)
                                          -> (Data.Monoid.<>)
                                               (Data.ProtoLens.Encoding.Bytes.putVarInt 74)
                                               ((Prelude..)
                                                  (\ bs
                                                     -> (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             (Prelude.fromIntegral
                                                                (Data.ByteString.length bs)))
                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                             bs))
                                                  Data.ProtoLens.encodeMessage
                                                  _v))
                                     (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                        (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))))
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
                               (Control.DeepSeq.deepseq
                                  (_Symbol'syntaxType x__)
                                  (Control.DeepSeq.deepseq
                                     (_Symbol'utf16CodeUnitSpan x__)
                                     (Control.DeepSeq.deepseq (_Symbol'byteRange x__) ())))))))))
newtype SyntaxType'UnrecognizedValue
  = SyntaxType'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data SyntaxType
  = FUNCTION |
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
  maybeToEnum 0 = Prelude.Just FUNCTION
  maybeToEnum 1 = Prelude.Just METHOD
  maybeToEnum 2 = Prelude.Just CLASS
  maybeToEnum 3 = Prelude.Just MODULE
  maybeToEnum 4 = Prelude.Just CALL
  maybeToEnum 5 = Prelude.Just TYPE
  maybeToEnum 6 = Prelude.Just INTERFACE
  maybeToEnum 7 = Prelude.Just IMPLEMENTATION
  maybeToEnum k
    = Prelude.Just
        (SyntaxType'Unrecognized
           (SyntaxType'UnrecognizedValue (Prelude.fromIntegral k)))
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
  minBound = FUNCTION
  maxBound = IMPLEMENTATION
instance Prelude.Enum SyntaxType where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum SyntaxType: " (Prelude.show k__)))
        Prelude.id
        (Data.ProtoLens.maybeToEnum k__)
  fromEnum FUNCTION = 0
  fromEnum METHOD = 1
  fromEnum CLASS = 2
  fromEnum MODULE = 3
  fromEnum CALL = 4
  fromEnum TYPE = 5
  fromEnum INTERFACE = 6
  fromEnum IMPLEMENTATION = 7
  fromEnum (SyntaxType'Unrecognized (SyntaxType'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ IMPLEMENTATION
    = Prelude.error
        "SyntaxType.succ: bad argument IMPLEMENTATION. This value would be out of bounds."
  succ FUNCTION = METHOD
  succ METHOD = CLASS
  succ CLASS = MODULE
  succ MODULE = CALL
  succ CALL = TYPE
  succ TYPE = INTERFACE
  succ INTERFACE = IMPLEMENTATION
  succ (SyntaxType'Unrecognized _)
    = Prelude.error "SyntaxType.succ: bad argument: unrecognized value"
  pred FUNCTION
    = Prelude.error
        "SyntaxType.pred: bad argument FUNCTION. This value would be out of bounds."
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
  fieldDefault = FUNCTION
instance Control.DeepSeq.NFData SyntaxType where
  rnf x__ = Prelude.seq x__ ()
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
    \\ENQfiles\CAN\SOH \ETX(\v2\NAK.github.semantic.FileR\ENQfiles\"@\n\
    \\DC1StackGraphRequest\DC2+\n\
    \\ENQblobs\CAN\SOH \ETX(\v2\NAK.github.semantic.BlobR\ENQblobs\"K\n\
    \\DC2StackGraphResponse\DC25\n\
    \\ENQfiles\CAN\SOH \ETX(\v2\US.github.semantic.StackGraphFileR\ENQfiles\"\"\n\
    \\n\
    \ParseError\DC2\DC4\n\
    \\ENQerror\CAN\SOH \SOH(\tR\ENQerror\"P\n\
    \\EOTBlob\DC2\CAN\n\
    \\acontent\CAN\SOH \SOH(\tR\acontent\DC2\DC2\n\
    \\EOTpath\CAN\STX \SOH(\tR\EOTpath\DC2\SUB\n\
    \\blanguage\CAN\ETX \SOH(\tR\blanguage\"\185\SOH\n\
    \\EOTFile\DC2\DC2\n\
    \\EOTpath\CAN\SOH \SOH(\tR\EOTpath\DC2\SUB\n\
    \\blanguage\CAN\STX \SOH(\tR\blanguage\DC21\n\
    \\asymbols\CAN\ETX \ETX(\v2\ETB.github.semantic.SymbolR\asymbols\DC23\n\
    \\ACKerrors\CAN\EOT \ETX(\v2\ESC.github.semantic.ParseErrorR\ACKerrors\DC2\EM\n\
    \\bblob_oid\CAN\ENQ \SOH(\tR\ablobOid\"\156\ETX\n\
    \\ACKSymbol\DC2\SYN\n\
    \\ACKsymbol\CAN\SOH \SOH(\tR\ACKsymbol\DC2\DC2\n\
    \\EOTkind\CAN\STX \SOH(\tR\EOTkind\DC2\DC2\n\
    \\EOTline\CAN\ETX \SOH(\tR\EOTline\DC2)\n\
    \\EOTspan\CAN\EOT \SOH(\v2\NAK.github.semantic.SpanR\EOTspan\DC2.\n\
    \\EOTdocs\CAN\ENQ \SOH(\v2\SUB.github.semantic.DocstringR\EOTdocs\DC26\n\
    \\tnode_type\CAN\ACK \SOH(\SO2\EM.github.semantic.NodeTypeR\bnodeType\DC2<\n\
    \\vsyntax_type\CAN\a \SOH(\SO2\ESC.github.semantic.SyntaxTypeR\n\
    \syntaxType\DC2F\n\
    \\DC4utf16_code_unit_span\CAN\b \SOH(\v2\NAK.github.semantic.SpanR\DC1utf16CodeUnitSpan\DC29\n\
    \\n\
    \byte_range\CAN\t \SOH(\v2\SUB.github.semantic.ByteRangeR\tbyteRange\")\n\
    \\tDocstring\DC2\FS\n\
    \\tdocstring\CAN\SOH \SOH(\tR\tdocstring\"6\n\
    \\bPosition\DC2\DC2\n\
    \\EOTline\CAN\SOH \SOH(\ENQR\EOTline\DC2\SYN\n\
    \\ACKcolumn\CAN\STX \SOH(\ENQR\ACKcolumn\"d\n\
    \\EOTSpan\DC2/\n\
    \\ENQstart\CAN\SOH \SOH(\v2\EM.github.semantic.PositionR\ENQstart\DC2+\n\
    \\ETXend\CAN\STX \SOH(\v2\EM.github.semantic.PositionR\ETXend\"3\n\
    \\tByteRange\DC2\DC4\n\
    \\ENQstart\CAN\SOH \SOH(\ENQR\ENQstart\DC2\DLE\n\
    \\ETXend\CAN\STX \SOH(\ENQR\ETXend\"\227\SOH\n\
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
    \\DC3ending_symbol_stack\CAN\a \ETX(\tR\DC1endingSymbolStack*`\n\
    \\bNodeType\DC2\SO\n\
    \\n\
    \ROOT_SCOPE\DLE\NUL\DC2\DC1\n\
    \\rJUMP_TO_SCOPE\DLE\SOH\DC2\DC2\n\
    \\SOEXPORTED_SCOPE\DLE\STX\DC2\SO\n\
    \\n\
    \DEFINITION\DLE\ETX\DC2\r\n\
    \\tREFERENCE\DLE\EOT*t\n\
    \\n\
    \SyntaxType\DC2\f\n\
    \\bFUNCTION\DLE\NUL\DC2\n\
    \\n\
    \\ACKMETHOD\DLE\SOH\DC2\t\n\
    \\ENQCLASS\DLE\STX\DC2\n\
    \\n\
    \\ACKMODULE\DLE\ETX\DC2\b\n\
    \\EOTCALL\DLE\EOT\DC2\b\n\
    \\EOTTYPE\DLE\ENQ\DC2\r\n\
    \\tINTERFACE\DLE\ACK\DC2\DC2\n\
    \\SOIMPLEMENTATION\DLE\aB\DC2\234\STX\SISemantic::ProtoJ\186$\n\
    \\ACK\DC2\EOT\NUL\NUL~\SOH\n\
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
    \\ETX\EOT\EOT\SOH\DC2\ETX\EM\b\EM\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX\SUB\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\EOT\DC2\ETX\SUB\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ACK\DC2\ETX\SUB\v\SI\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX\SUB\DLE\NAK\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX\SUB\CAN\EM\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT\GS\NUL\US\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX\GS\b\SUB\n\
    \\v\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\ETX\RS\STX$\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\EOT\DC2\ETX\RS\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ACK\DC2\ETX\RS\v\EM\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\ETX\RS\SUB\US\n\
    \\f\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\ETX\RS\"#\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOT!\NUL#\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX!\b\DC2\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX\"\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\EOT\DC2\EOT\"\STX!\DC4\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX\"\STX\b\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX\"\t\SO\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX\"\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\a\DC2\EOT%\NUL)\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX%\b\f\n\
    \\v\n\
    \\EOT\EOT\a\STX\NUL\DC2\ETX&\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\EOT\DC2\EOT&\STX%\SO\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\ETX&\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\ETX&\t\DLE\n\
    \\f\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\ETX&\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\a\STX\SOH\DC2\ETX'\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\EOT\DC2\EOT'\STX&\NAK\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\ETX'\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\ETX'\t\r\n\
    \\f\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\ETX'\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\a\STX\STX\DC2\ETX(\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\EOT\DC2\EOT(\STX'\DC2\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ENQ\DC2\ETX(\STX\b\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\ETX(\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\ETX(\DC4\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\b\DC2\EOT+\NUL1\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\b\SOH\DC2\ETX+\b\f\n\
    \\v\n\
    \\EOT\EOT\b\STX\NUL\DC2\ETX,\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\EOT\DC2\EOT,\STX+\SO\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ENQ\DC2\ETX,\STX\b\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\ETX,\t\r\n\
    \\f\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\ETX,\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\b\STX\SOH\DC2\ETX-\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\EOT\DC2\EOT-\STX,\DC2\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ENQ\DC2\ETX-\STX\b\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\ETX-\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\ETX-\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\b\STX\STX\DC2\ETX.\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\EOT\DC2\ETX.\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\ACK\DC2\ETX.\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\SOH\DC2\ETX.\DC2\EM\n\
    \\f\n\
    \\ENQ\EOT\b\STX\STX\ETX\DC2\ETX.\FS\GS\n\
    \\v\n\
    \\EOT\EOT\b\STX\ETX\DC2\ETX/\STX!\n\
    \\f\n\
    \\ENQ\EOT\b\STX\ETX\EOT\DC2\ETX/\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\b\STX\ETX\ACK\DC2\ETX/\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\b\STX\ETX\SOH\DC2\ETX/\SYN\FS\n\
    \\f\n\
    \\ENQ\EOT\b\STX\ETX\ETX\DC2\ETX/\US \n\
    \\v\n\
    \\EOT\EOT\b\STX\EOT\DC2\ETX0\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\b\STX\EOT\EOT\DC2\EOT0\STX/!\n\
    \\f\n\
    \\ENQ\EOT\b\STX\EOT\ENQ\DC2\ETX0\STX\b\n\
    \\f\n\
    \\ENQ\EOT\b\STX\EOT\SOH\DC2\ETX0\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\b\STX\EOT\ETX\DC2\ETX0\DC4\NAK\n\
    \\n\
    \\n\
    \\STX\EOT\t\DC2\EOT3\NUL=\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\t\SOH\DC2\ETX3\b\SO\n\
    \\v\n\
    \\EOT\EOT\t\STX\NUL\DC2\ETX4\STX\DC4\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\EOT\DC2\EOT4\STX3\DLE\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\ETX4\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\ETX4\t\SI\n\
    \\f\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\ETX4\DC2\DC3\n\
    \\v\n\
    \\EOT\EOT\t\STX\SOH\DC2\ETX5\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\EOT\DC2\EOT5\STX4\DC4\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\ETX5\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\ETX5\t\r\n\
    \\f\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\ETX5\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\t\STX\STX\DC2\ETX6\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\EOT\DC2\EOT6\STX5\DC2\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ENQ\DC2\ETX6\STX\b\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\ETX6\t\r\n\
    \\f\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\ETX6\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\t\STX\ETX\DC2\ETX7\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\EOT\DC2\EOT7\STX6\DC2\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ETX\ACK\DC2\ETX7\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ETX\SOH\DC2\ETX7\a\v\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ETX\ETX\DC2\ETX7\SO\SI\n\
    \\v\n\
    \\EOT\EOT\t\STX\EOT\DC2\ETX8\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\EOT\DC2\EOT8\STX7\DLE\n\
    \\f\n\
    \\ENQ\EOT\t\STX\EOT\ACK\DC2\ETX8\STX\v\n\
    \\f\n\
    \\ENQ\EOT\t\STX\EOT\SOH\DC2\ETX8\f\DLE\n\
    \\f\n\
    \\ENQ\EOT\t\STX\EOT\ETX\DC2\ETX8\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\t\STX\ENQ\DC2\ETX9\STX\EM\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ENQ\EOT\DC2\EOT9\STX8\NAK\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ENQ\ACK\DC2\ETX9\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ENQ\SOH\DC2\ETX9\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ENQ\ETX\DC2\ETX9\ETB\CAN\n\
    \\v\n\
    \\EOT\EOT\t\STX\ACK\DC2\ETX:\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ACK\EOT\DC2\EOT:\STX9\EM\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ACK\ACK\DC2\ETX:\STX\f\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ACK\SOH\DC2\ETX:\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\t\STX\ACK\ETX\DC2\ETX:\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\t\STX\a\DC2\ETX;\STX \n\
    \\r\n\
    \\ENQ\EOT\t\STX\a\EOT\DC2\EOT;\STX:\GS\n\
    \\f\n\
    \\ENQ\EOT\t\STX\a\ACK\DC2\ETX;\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\t\STX\a\SOH\DC2\ETX;\a\ESC\n\
    \\f\n\
    \\ENQ\EOT\t\STX\a\ETX\DC2\ETX;\RS\US\n\
    \\v\n\
    \\EOT\EOT\t\STX\b\DC2\ETX<\STX\ESC\n\
    \\r\n\
    \\ENQ\EOT\t\STX\b\EOT\DC2\EOT<\STX; \n\
    \\f\n\
    \\ENQ\EOT\t\STX\b\ACK\DC2\ETX<\STX\v\n\
    \\f\n\
    \\ENQ\EOT\t\STX\b\SOH\DC2\ETX<\f\SYN\n\
    \\f\n\
    \\ENQ\EOT\t\STX\b\ETX\DC2\ETX<\EM\SUB\n\
    \\n\
    \\n\
    \\STX\EOT\n\
    \\DC2\EOT?\NULA\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\n\
    \\SOH\DC2\ETX?\b\DC1\n\
    \\v\n\
    \\EOT\EOT\n\
    \\STX\NUL\DC2\ETX@\STX\ETB\n\
    \\r\n\
    \\ENQ\EOT\n\
    \\STX\NUL\EOT\DC2\EOT@\STX?\DC3\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ENQ\DC2\ETX@\STX\b\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\SOH\DC2\ETX@\t\DC2\n\
    \\f\n\
    \\ENQ\EOT\n\
    \\STX\NUL\ETX\DC2\ETX@\NAK\SYN\n\
    \\n\
    \\n\
    \\STX\EOT\v\DC2\EOTC\NULF\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\v\SOH\DC2\ETXC\b\DLE\n\
    \\v\n\
    \\EOT\EOT\v\STX\NUL\DC2\ETXD\STX\DC1\n\
    \\r\n\
    \\ENQ\EOT\v\STX\NUL\EOT\DC2\EOTD\STXC\DC2\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ENQ\DC2\ETXD\STX\a\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\SOH\DC2\ETXD\b\f\n\
    \\f\n\
    \\ENQ\EOT\v\STX\NUL\ETX\DC2\ETXD\SI\DLE\n\
    \\v\n\
    \\EOT\EOT\v\STX\SOH\DC2\ETXE\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\v\STX\SOH\EOT\DC2\EOTE\STXD\DC1\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ENQ\DC2\ETXE\STX\a\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\SOH\DC2\ETXE\b\SO\n\
    \\f\n\
    \\ENQ\EOT\v\STX\SOH\ETX\DC2\ETXE\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\f\DC2\EOTH\NULK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\f\SOH\DC2\ETXH\b\f\n\
    \\v\n\
    \\EOT\EOT\f\STX\NUL\DC2\ETXI\STX\NAK\n\
    \\r\n\
    \\ENQ\EOT\f\STX\NUL\EOT\DC2\EOTI\STXH\SO\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ACK\DC2\ETXI\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\SOH\DC2\ETXI\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\f\STX\NUL\ETX\DC2\ETXI\DC3\DC4\n\
    \\v\n\
    \\EOT\EOT\f\STX\SOH\DC2\ETXJ\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\f\STX\SOH\EOT\DC2\EOTJ\STXI\NAK\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ACK\DC2\ETXJ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\SOH\DC2\ETXJ\v\SO\n\
    \\f\n\
    \\ENQ\EOT\f\STX\SOH\ETX\DC2\ETXJ\DC1\DC2\n\
    \\n\
    \\n\
    \\STX\EOT\r\DC2\EOTM\NULP\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\r\SOH\DC2\ETXM\b\DC1\n\
    \\v\n\
    \\EOT\EOT\r\STX\NUL\DC2\ETXN\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\r\STX\NUL\EOT\DC2\EOTN\STXM\DC3\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ENQ\DC2\ETXN\STX\a\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\SOH\DC2\ETXN\b\r\n\
    \\f\n\
    \\ENQ\EOT\r\STX\NUL\ETX\DC2\ETXN\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\r\STX\SOH\DC2\ETXO\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\r\STX\SOH\EOT\DC2\EOTO\STXN\DC2\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ENQ\DC2\ETXO\STX\a\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\SOH\DC2\ETXO\b\v\n\
    \\f\n\
    \\ENQ\EOT\r\STX\SOH\ETX\DC2\ETXO\SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\SO\DC2\EOTR\NULX\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SO\SOH\DC2\ETXR\b\SYN\n\
    \\v\n\
    \\EOT\EOT\SO\STX\NUL\DC2\ETXS\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\SO\STX\NUL\EOT\DC2\EOTS\STXR\CAN\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ENQ\DC2\ETXS\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\SOH\DC2\ETXS\t\r\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\NUL\ETX\DC2\ETXS\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\SO\STX\SOH\DC2\ETXT\STX\SYN\n\
    \\r\n\
    \\ENQ\EOT\SO\STX\SOH\EOT\DC2\EOTT\STXS\DC2\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ENQ\DC2\ETXT\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\SOH\DC2\ETXT\t\DC1\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\SOH\ETX\DC2\ETXT\DC4\NAK\n\
    \\v\n\
    \\EOT\EOT\SO\STX\STX\DC2\ETXU\STX$\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\EOT\DC2\ETXU\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\ACK\DC2\ETXU\v\EM\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\SOH\DC2\ETXU\SUB\US\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\STX\ETX\DC2\ETXU\"#\n\
    \\v\n\
    \\EOT\EOT\SO\STX\ETX\DC2\ETXV\STX$\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\EOT\DC2\ETXV\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\ACK\DC2\ETXV\v\EM\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\SOH\DC2\ETXV\SUB\US\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\ETX\ETX\DC2\ETXV\"#\n\
    \\v\n\
    \\EOT\EOT\SO\STX\EOT\DC2\ETXW\STX!\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\EOT\DC2\ETXW\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\ACK\DC2\ETXW\v\NAK\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\SOH\DC2\ETXW\SYN\FS\n\
    \\f\n\
    \\ENQ\EOT\SO\STX\EOT\ETX\DC2\ETXW\US \n\
    \\n\
    \\n\
    \\STX\ENQ\NUL\DC2\EOTZ\NUL`\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETXZ\ENQ\r\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETX[\STX\DC1\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETX[\STX\f\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETX[\SI\DLE\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\ETX\\\STX\DC4\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\ETX\\\STX\SI\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\ETX\\\DC2\DC3\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\ETX]\STX\NAK\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\ETX]\STX\DLE\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\ETX]\DC3\DC4\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ETX\DC2\ETX^\STX\DC1\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\SOH\DC2\ETX^\STX\f\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\STX\DC2\ETX^\SI\DLE\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\EOT\DC2\ETX_\STX\DLE\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\SOH\DC2\ETX_\STX\v\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\STX\DC2\ETX_\SO\SI\n\
    \\n\
    \\n\
    \\STX\ENQ\SOH\DC2\EOTb\NULk\SOH\n\
    \\n\
    \\n\
    \\ETX\ENQ\SOH\SOH\DC2\ETXb\ENQ\SI\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\NUL\DC2\ETXc\STX\SI\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\SOH\DC2\ETXc\STX\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\STX\DC2\ETXc\r\SO\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\SOH\DC2\ETXd\STX\r\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\SOH\DC2\ETXd\STX\b\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\STX\DC2\ETXd\v\f\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\STX\DC2\ETXe\STX\f\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\SOH\DC2\ETXe\STX\a\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\STX\STX\DC2\ETXe\n\
    \\v\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\ETX\DC2\ETXf\STX\r\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\ETX\SOH\DC2\ETXf\STX\b\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\ETX\STX\DC2\ETXf\v\f\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\EOT\DC2\ETXg\STX\v\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\EOT\SOH\DC2\ETXg\STX\ACK\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\EOT\STX\DC2\ETXg\t\n\
    \\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\ENQ\DC2\ETXh\STX\v\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\ENQ\SOH\DC2\ETXh\STX\ACK\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\ENQ\STX\DC2\ETXh\t\n\
    \\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\ACK\DC2\ETXi\STX\DLE\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\ACK\SOH\DC2\ETXi\STX\v\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\ACK\STX\DC2\ETXi\SO\SI\n\
    \\v\n\
    \\EOT\ENQ\SOH\STX\a\DC2\ETXj\STX\NAK\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\a\SOH\DC2\ETXj\STX\DLE\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\a\STX\DC2\ETXj\DC3\DC4\n\
    \\n\
    \\n\
    \\STX\EOT\SI\DC2\EOTm\NULt\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SI\SOH\DC2\ETXm\b\SYN\n\
    \\v\n\
    \\EOT\EOT\SI\STX\NUL\DC2\ETXn\STX\SI\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\NUL\EOT\DC2\EOTn\STXm\CAN\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ENQ\DC2\ETXn\STX\a\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\SOH\DC2\ETXn\b\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\NUL\ETX\DC2\ETXn\r\SO\n\
    \\v\n\
    \\EOT\EOT\SI\STX\SOH\DC2\ETXo\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\SOH\EOT\DC2\EOTo\STXn\SI\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ENQ\DC2\ETXo\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\SOH\DC2\ETXo\t\r\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\SOH\ETX\DC2\ETXo\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\SI\STX\STX\DC2\ETXp\STX\DC2\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\STX\EOT\DC2\EOTp\STXo\DC2\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\STX\ENQ\DC2\ETXp\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\STX\SOH\DC2\ETXp\t\r\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\STX\ETX\DC2\ETXp\DLE\DC1\n\
    \\v\n\
    \\EOT\EOT\SI\STX\ETX\DC2\ETXq\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\ETX\EOT\DC2\EOTq\STXp\DC2\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ETX\ACK\DC2\ETXq\STX\ACK\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ETX\SOH\DC2\ETXq\a\v\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ETX\ETX\DC2\ETXq\SO\SI\n\
    \\v\n\
    \\EOT\EOT\SI\STX\EOT\DC2\ETXr\STX\GS\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\EOT\EOT\DC2\EOTr\STXq\DLE\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\EOT\ACK\DC2\ETXr\STX\f\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\EOT\SOH\DC2\ETXr\r\CAN\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\EOT\ETX\DC2\ETXr\ESC\FS\n\
    \\v\n\
    \\EOT\EOT\SI\STX\ENQ\DC2\ETXs\STX\EM\n\
    \\r\n\
    \\ENQ\EOT\SI\STX\ENQ\EOT\DC2\EOTs\STXr\GS\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ENQ\ACK\DC2\ETXs\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ENQ\SOH\DC2\ETXs\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\SI\STX\ENQ\ETX\DC2\ETXs\ETB\CAN\n\
    \\n\
    \\n\
    \\STX\EOT\DLE\DC2\EOTv\NUL~\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\DLE\SOH\DC2\ETXv\b\SYN\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\NUL\DC2\ETXw\STX,\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\EOT\DC2\ETXw\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\ENQ\DC2\ETXw\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\SOH\DC2\ETXw\DC2'\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\NUL\ETX\DC2\ETXw*+\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\SOH\DC2\ETXx\STX&\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\SOH\EOT\DC2\EOTx\STXw,\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\ENQ\DC2\ETXx\STX\a\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\SOH\DC2\ETXx\b!\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\SOH\ETX\DC2\ETXx$%\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\STX\DC2\ETXy\STX\DC1\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\STX\EOT\DC2\EOTy\STXx&\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\STX\ENQ\DC2\ETXy\STX\a\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\STX\SOH\DC2\ETXy\b\f\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\STX\ETX\DC2\ETXy\SI\DLE\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\ETX\DC2\ETXz\STX\DC3\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\ETX\EOT\DC2\EOTz\STXy\DC1\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ETX\ENQ\DC2\ETXz\STX\b\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ETX\SOH\DC2\ETXz\t\SO\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ETX\ETX\DC2\ETXz\DC1\DC2\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\EOT\DC2\ETX{\STX\SI\n\
    \\r\n\
    \\ENQ\EOT\DLE\STX\EOT\EOT\DC2\EOT{\STXz\DC3\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\EOT\ENQ\DC2\ETX{\STX\a\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\EOT\SOH\DC2\ETX{\b\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\EOT\ETX\DC2\ETX{\r\SO\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\ENQ\DC2\ETX|\STX(\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ENQ\EOT\DC2\ETX|\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ENQ\ENQ\DC2\ETX|\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ENQ\SOH\DC2\ETX|\DC1#\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ENQ\ETX\DC2\ETX|&'\n\
    \\v\n\
    \\EOT\EOT\DLE\STX\ACK\DC2\ETX}\STX*\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ACK\EOT\DC2\ETX}\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ACK\ENQ\DC2\ETX}\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ACK\SOH\DC2\ETX}\DC2%\n\
    \\f\n\
    \\ENQ\EOT\DLE\STX\ACK\ETX\DC2\ETX}()b\ACKproto3"