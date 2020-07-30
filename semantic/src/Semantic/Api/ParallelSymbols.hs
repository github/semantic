{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Semantic.Api.ParallelSymbols
  ( parseSymbols,
    parseSymbolsBuilder,
    tagsForBlob,
  )
where

import           Control.Effect.Error
import           Control.Effect.Parse.Parallel
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
import           Proto.Semantic as P hiding (Blob, ParseError (..))
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import           Semantic.Api.Bridge
import           Semantic.Config
import           Semantic.Task
import           Serializing.Format (Format)
import           Source.Language
import           Source.Loc as Loc
import           Tags.Tagging.Precise

parseSymbolsBuilder :: (Has (Error SomeException) sig m, Has ParallelParse sig m, Has (Reader Config) sig m, Traversable t) => Format ParseTreeSymbolResponse -> t Blob -> m Builder
parseSymbolsBuilder format blobs = parseSymbols blobs >>= serialize format

parseSymbols :: (Has (Error SomeException) sig m, Has ParallelParse sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseSymbols blobs = do
  terms <- tagsForBlobs blobs

--   files <- traverse
--   pure $ defMessage & P.files .~ toList (fmap go terms)
--   where
--     errorFile err = let blob = errorBlob err in
--       defMessage
--         & P.path .~ (pack $ blobFilePath blob)
--         & P.language .~ (bridging # blobLanguage blob)
--         & P.symbols .~ mempty
--         & P.errors .~ [defMessage & P.error .~ pack (show e)]
--     go :: (Has (Error SomeException) sig m, Has ParallelParse sig m) => TagResult -> m File
--     go = \case
--       Failed err -> errorFile err
--       Ok blob tags -> tagsToFile blob tags
--         where

--           tagsToFile :: Blob -> [Tag] -> File
--           tagsToFile blob tags =
--             defMessage
--               & P.path .~ (pack $ blobFilePath blob)
--               & P.language .~ (bridging # blobLanguage blob)
--               & P.symbols .~ fmap tagToSymbol tags
--               & P.errors .~ mempty
--           tagToSymbol :: Tag -> Symbol
--           tagToSymbol tag =
--             defMessage
--               & P.symbol .~ tagName tag
--               & P.kind .~ toKind tag
--               & P.nodeType .~ tagNodeType tag
--               & P.syntaxType .~ tagSyntaxType tag
--               & P.line .~ tagLine tag
--               & P.maybe'span ?~ converting # unOneIndexedSpan (tagOneIndexedSpan tag)
--               & P.maybe'utf16CodeUnitSpan ?~ converting # unUTF16CodeUnitSpan (tagUTF16CodeUnitSpan tag)
--               & P.byteRange .~ bridging # tagByteRange tag
--             where
--               toKind = toTitle . pack . show . tagSyntaxType

data TagResult
  = Failed ParseError
  | Ok Blob [Tag]

tagsForBlobs :: (Has (Error SomeException) sig m, Has ParallelParse sig m, Traversable t) => t Blob -> m (t TagResult)
tagsForBlobs = fmap (fmap unify) <$> parseWith toTagsParsers (\blob -> pure . go blob)
  where
    unify = either Failed Prelude.id
    go blob term = Ok blob (tags (blobSource blob) term)
    toTagsParsers :: Map Language (Parser.SomeParser ToTags Loc)
    toTagsParsers = Parser.preciseParsers
