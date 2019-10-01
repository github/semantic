{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.Api.Symbols
  ( legacyParseSymbols
  , parseSymbols
  , parseSymbolsBuilder
  ) where

import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Data.Blob hiding (File (..))
import           Data.ByteString.Builder
import           Data.Language
import           Data.Term
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Text (pack)
import           Parsing.Parser
import           Prologue
import           Semantic.Api.Bridge
import qualified Semantic.Api.LegacyTypes as Legacy
import           Semantic.Api.Terms (ParseEffects, doParse)
import           Semantic.Proto.SemanticPB hiding (Blob)
import           Semantic.Task
import           Serializing.Format
import           Source.Loc
import           Tags.Taggable
import           Tags.Tagging
import qualified Tags.Tagging.Precise as Precise

legacyParseSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m Legacy.ParseTreeSymbolResponse
legacyParseSymbols blobs = Legacy.ParseTreeSymbolResponse <$> distributeFoldMap go blobs
  where
    go :: ParseEffects sig m => Blob -> m [Legacy.File]
    go blob@Blob{..} = (doParse blob >>= withSomeTerm renderToSymbols) `catchError` (\(SomeException _) -> pure (pure emptyFile))
      where
        emptyFile = tagsToFile []

        -- Legacy symbols output doesn't include Function Calls.
        symbolsToSummarize :: [Text]
        symbolsToSummarize = ["Function", "Method", "Class", "Module"]

        renderToSymbols :: (IsTaggable f, Applicative m) => Term f Loc -> m [Legacy.File]
        renderToSymbols = pure . pure . tagsToFile . runTagging (blobLanguage blob) blobSource symbolsToSummarize

        tagsToFile :: [Tag] -> Legacy.File
        tagsToFile tags = Legacy.File (pack (blobPath blob)) (pack (show (blobLanguage blob))) (fmap tagToSymbol tags)

        tagToSymbol :: Tag -> Legacy.Symbol
        tagToSymbol Tag{..}
          = Legacy.Symbol
          { symbolName = name
          , symbolKind = pack (show kind)
          , symbolLine = line
          , symbolSpan = converting #? span
          }

parseSymbolsBuilder :: (Member Distribute sig, ParseEffects sig m, Traversable t) => Format ParseTreeSymbolResponse -> t Blob -> m Builder
parseSymbolsBuilder format blobs = parseSymbols blobs >>= serialize format

parseSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseSymbols blobs = do
  modes <- ask
  ParseTreeSymbolResponse . V.fromList . toList <$> distributeFor blobs (go modes)
  where
    go :: ParseEffects sig m => PerLanguageModes -> Blob -> m File
    go modes blob@Blob{..}
      | Precise <- pythonMode modes
      , Python  <- blobLanguage'
      =             catching $ renderPreciseToSymbols       <$> parse precisePythonParser blob
      | otherwise = catching $ withSomeTerm renderToSymbols <$> doParse                   blob
      where
        catching m = m `catchError` (\(SomeException e) -> pure $ errorFile (show e))
        blobLanguage' = blobLanguage blob
        blobPath' = pack $ blobPath blob
        errorFile e = File blobPath' (bridging # blobLanguage') mempty (V.fromList [ParseError (T.pack e)]) blobOid

        renderToSymbols :: IsTaggable f => Term f Loc -> File
        renderToSymbols term = renderPreciseToSymbols (ALaCarteTerm (blobLanguage blob) term)

        renderPreciseToSymbols :: Precise.ToTags t => t Loc -> File
        renderPreciseToSymbols term = tagsToFile (Precise.tags blobSource term)

        tagsToFile :: [Tag] -> File
        tagsToFile tags = File blobPath' (bridging # blobLanguage') (V.fromList (fmap tagToSymbol tags)) mempty blobOid

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module", "Call", "Send"]

tagToSymbol :: Tag -> Symbol
tagToSymbol Tag{..} = Symbol
  { symbol = name
  , kind = pack (show kind)
  , line = line
  , span = converting #? span
  , docs = fmap Docstring docs
  }


data ALaCarteTerm syntax ann = ALaCarteTerm Language (Term syntax ann)

instance IsTaggable syntax => Precise.ToTags (ALaCarteTerm syntax) where
  tags source (ALaCarteTerm lang term) = runTagging lang source symbolsToSummarize term
