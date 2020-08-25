{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Semantic.Api.Symbols
  ( parseSymbols,
    parseSymbolsBuilder,
    tagsForBlob,
  )
where

import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Foldable
import           Data.Map.Strict (Map)
import           Data.ProtoLens (defMessage)
import           Data.Text (pack, toTitle)
import qualified Parsing.Parser as Parser
import           Proto.Semantic as P hiding (Blob)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import           Semantic.Api.Bridge
import           Semantic.Config
import           Semantic.Task
import           Serializing.Format (Format)
import           Source.Language
import           Source.Loc as Loc
import           Tags.Tagging.Precise

parseSymbolsBuilder :: (Has (Error SomeException) sig m, Has Parse sig m, Has (Reader Config) sig m, Traversable t) => Format ParseTreeSymbolResponse -> t Blob -> m Builder
parseSymbolsBuilder format blobs = parseSymbols blobs >>= serialize format

parseSymbols :: (Has (Error SomeException) sig m, Has Parse sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseSymbols blobs = do
  terms <- traverse go blobs
  pure $ defMessage & P.files .~ toList terms
  where
    go :: (Has (Error SomeException) sig m, Has Parse sig m) => Blob -> m File
    go blob = catching $ tagsToFile <$> tagsForBlob blob
      where
        catching m = m `catchError` (\(SomeException e) -> pure $ errorFile (show e))
        blobLanguage' = blobLanguage blob
        blobPath' = pack $ blobFilePath blob
        errorFile e =
          defMessage
            & P.path .~ blobPath'
            & P.language .~ (bridging # blobLanguage')
            & P.symbols .~ mempty
            & P.errors .~ [defMessage & P.error .~ pack e]
        tagsToFile :: [Tag] -> File
        tagsToFile tags =
          defMessage
            & P.path .~ blobPath'
            & P.language .~ (bridging # blobLanguage')
            & P.symbols .~ fmap tagToSymbol tags
            & P.errors .~ mempty
        tagToSymbol :: Tag -> Symbol
        tagToSymbol tag =
          defMessage
            & P.symbol .~ tagName tag
            & P.kind .~ toKind tag
            & P.nodeType .~ tagNodeType tag
            & P.syntaxType .~ tagSyntaxType tag
            & P.line .~ tagLine tag
            & P.maybe'span ?~ converting # unOneIndexedSpan (tagOneIndexedSpan tag)
            & P.maybe'utf16CodeUnitSpan ?~ converting # unUTF16CodeUnitSpan (tagUTF16CodeUnitSpan tag)
            & P.byteRange .~ bridging # tagByteRange tag
          where
            toKind = toTitle . pack . show . tagSyntaxType

tagsForBlob :: (Has (Error SomeException) sig m, Has Parse sig m) => Blob -> m [Tag]
tagsForBlob blob = parseWith toTagsParsers (pure . tags (blobSource blob)) blob
  where
    toTagsParsers :: Map Language (Parser.SomeParser ToTags Loc)
    toTagsParsers = Parser.preciseParsers
