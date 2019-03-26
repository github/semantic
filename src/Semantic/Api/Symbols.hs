{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.Api.Symbols
  ( legacyParseSymbols
  , parseSymbols
  , parseSymbolsBuilder
  ) where

import Prelude hiding (span)

import           Control.Effect
import           Control.Effect.Error
import           Control.Exception
import           Control.Lens
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Location
import           Data.Maybe
import           Data.Term
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Text (pack)
import           Parsing.Parser
import           Prologue
import           Semantic.Api.Bridge
import qualified Semantic.Api.LegacyTypes as Legacy
import           Semantic.Api.Terms (ParseEffects, doParse)
import           Semantic.Api.V1.CodeAnalysisPB hiding (Blob)
import           Semantic.Task
import           Serializing.Format
import           Tags.Taggable
import           Tags.Tagging

legacyParseSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m Legacy.ParseTreeSymbolResponse
legacyParseSymbols blobs = Legacy.ParseTreeSymbolResponse <$> distributeFoldMap go blobs
  where
    go :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m) => Blob -> m [Legacy.File]
    go blob@Blob{..} = (doParse blob >>= withSomeTerm (renderToSymbols blob)) `catchError` (\(SomeException _) -> pure (pure emptyFile))
      where emptyFile = Legacy.File (pack blobPath) (pack (show blobLanguage)) []

    -- Legacy symbols output doesn't include Function Calls.
    symbolsToSummarize :: [Text]
    symbolsToSummarize = ["Function", "Method", "Class", "Module"]

    renderToSymbols :: (IsTaggable f, Applicative m) => Blob -> Term f Location -> m [Legacy.File]
    renderToSymbols blob term = pure $ either mempty (pure . tagsToFile blob) (runTagging blob symbolsToSummarize term)

    tagsToFile :: Blob -> [Tag] -> Legacy.File
    tagsToFile Blob{..} tags = Legacy.File (pack blobPath) (pack (show blobLanguage)) (fmap tagToSymbol tags)

    tagToSymbol :: Tag -> Legacy.Symbol
    tagToSymbol Tag{..}
      = Legacy.Symbol
      { symbolName = name
      , symbolKind = kind
      , symbolLine = fromMaybe mempty line
      , symbolSpan = converting #? span
      }

parseSymbolsBuilder :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m Builder
parseSymbolsBuilder blobs = parseSymbols blobs >>= serialize JSON

parseSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseSymbols blobs = ParseTreeSymbolResponse . V.fromList . toList <$> distributeFor blobs go
  where
    go :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m) => Blob -> m File
    go blob@Blob{..} = (doParse blob >>= withSomeTerm (renderToSymbols blob)) `catchError` (\(SomeException e) -> pure $ errorFile (show e))
      where
        errorFile e = File (pack blobPath) (bridging # blobLanguage) mempty (V.fromList [ParseError (T.pack e)]) blobOid

        symbolsToSummarize :: [Text]
        symbolsToSummarize = ["Function", "Method", "Class", "Module", "Call", "Send"]

        renderToSymbols :: (IsTaggable f, Applicative m) => Blob -> Term f Location -> m File
        renderToSymbols blob@Blob{..} term = pure $ either (errorFile . show) (tagsToFile blob) (runTagging blob symbolsToSummarize term)

        tagsToFile :: Blob -> [Tag] -> File
        tagsToFile Blob{..} tags = File (pack blobPath) (bridging # blobLanguage) (V.fromList (fmap tagToSymbol tags)) mempty blobOid

        tagToSymbol :: Tag -> Symbol
        tagToSymbol Tag{..}
          = Symbol
          { symbol = name
          , kind = kind
          , line = fromMaybe mempty line
          , span = converting #? span
          , docs = fmap Docstring docs
          }
