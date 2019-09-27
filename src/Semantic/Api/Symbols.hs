{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.Api.Symbols
  ( legacyParseSymbols
  , parseSymbols
  , parseSymbolsBuilder
  ) where

import           Control.Effect.Error
import           Control.Exception
import           Data.Blob hiding (File (..))
import           Data.ByteString.Builder
import           Data.Maybe
import           Data.Term
import qualified Data.Text as T
import           Data.Text (pack)
import           Parsing.Parser
import           Prologue
import           Semantic.Api.Bridge
import qualified Semantic.Api.LegacyTypes as Legacy
import           Semantic.Api.Terms (ParseEffects, doParse)
import           Semantic.Task
import           Serializing.Format
import           Source.Loc
import           Tags.Taggable
import           Tags.Tagging

import           Control.Lens
import           Data.ProtoLens (defMessage)
import           Proto.Semantic as P hiding (Blob)
import           Proto.Semantic_Fields as P

legacyParseSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m Legacy.ParseTreeSymbolResponse
legacyParseSymbols blobs = Legacy.ParseTreeSymbolResponse <$> distributeFoldMap go blobs
  where
    go :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m) => Blob -> m [Legacy.File]
    go blob@Blob{..} = (doParse blob >>= withSomeTerm renderToSymbols) `catchError` (\(SomeException _) -> pure (pure emptyFile))
      where
        emptyFile = tagsToFile []

        -- Legacy symbols output doesn't include Function Calls.
        symbolsToSummarize :: [Text]
        symbolsToSummarize = ["Function", "Method", "Class", "Module"]

        renderToSymbols :: (IsTaggable f, Applicative m) => Term f Loc -> m [Legacy.File]
        renderToSymbols = pure . pure . tagsToFile . runTagging blob symbolsToSummarize

        tagsToFile :: [Tag] -> Legacy.File
        tagsToFile tags = Legacy.File (pack (blobPath blob)) (pack (show (blobLanguage blob))) (fmap tagToSymbol tags)

        tagToSymbol :: Tag -> Legacy.Symbol
        tagToSymbol Tag{..}
          = Legacy.Symbol
          { symbolName = name
          , symbolKind = kind
          , symbolLine = fromMaybe mempty line
          , symbolSpan = converting #? span
          }

parseSymbolsBuilder :: (Member Distribute sig, ParseEffects sig m, Traversable t) => Format ParseTreeSymbolResponse -> t Blob -> m Builder
parseSymbolsBuilder format blobs = parseSymbols blobs >>= serialize format

parseSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseSymbols blobs = do -- ParseTreeSymbolResponse . V.fromList . toList <$> distributeFor blobs go
  terms <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList terms
  where
    go :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m) => Blob -> m File
    go blob@Blob{..} = (doParse blob >>= withSomeTerm renderToSymbols) `catchError` (\(SomeException e) -> pure $ errorFile (show e))
      where
        blobLanguage' = blobLanguage blob
        blobPath' = pack $ blobPath blob
        errorFile e = defMessage
          & P.path .~ blobPath'
          & P.language .~ (bridging # blobLanguage blob)
          & P.symbols .~ mempty
          & P.errors .~ [defMessage & P.error .~ T.pack e]
          & P.blobOid .~ blobOid

        symbolsToSummarize :: [Text]
        symbolsToSummarize = ["Function", "Method", "Class", "Module", "Call", "Send"]

        renderToSymbols :: (IsTaggable f, Applicative m) => Term f Loc -> m File
        renderToSymbols term = pure $ tagsToFile (runTagging blob symbolsToSummarize term)

        tagsToFile :: [Tag] -> File
        tagsToFile tags = defMessage
          & P.path .~ blobPath'
          & P.language .~ (bridging # blobLanguage')
          & P.symbols .~ fmap tagToSymbol tags
          & P.errors .~ mempty
          & P.blobOid .~ blobOid

        tagToSymbol :: Tag -> Symbol
        tagToSymbol Tag{..} = defMessage
          & P.symbol .~ name
          & P.kind .~ kind
          & P.line .~ fromMaybe mempty line
          & P.maybe'span .~ converting #? span
          & P.maybe'docs .~ case docs of
            Just d -> Just (defMessage & P.docstring .~ d)
            Nothing -> Nothing
