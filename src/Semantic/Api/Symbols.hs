{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, TypeOperators #-}
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
import qualified Parsing.Parser as Parser
import           Prologue
import           Semantic.Api.Bridge
import qualified Semantic.Api.LegacyTypes as Legacy
import           Semantic.Proto.SemanticPB hiding (Blob)
import           Semantic.Config
import           Semantic.Task
import           Serializing.Format (Format)
import           Source.Loc
import           Tags.Taggable
import           Tags.Tagging
import qualified Tags.Tagging.Precise as Precise

legacyParseSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m Legacy.ParseTreeSymbolResponse
legacyParseSymbols blobs = Legacy.ParseTreeSymbolResponse <$> distributeFoldMap go blobs
  where
    go :: ParseEffects sig m => Blob -> m [Legacy.File]
    go blob@Blob{..} = (withSomeTerm renderToSymbols <$> doParse symbolsToSummarize blob) `catchError` (\(SomeException _) -> pure (pure emptyFile))
      where
        emptyFile = tagsToFile []

        -- Legacy symbols output doesn't include Function Calls.
        symbolsToSummarize :: [Text]
        symbolsToSummarize = ["Function", "Method", "Class", "Module"]

        renderToSymbols :: Precise.ToTags t => t Loc -> [Legacy.File]
        renderToSymbols = pure . tagsToFile . Precise.tags blobSource

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
parseSymbols blobs = ParseTreeSymbolResponse . V.fromList . toList <$> distributeFor blobs go
  where
    go :: ParseEffects sig m => Blob -> m File
    go blob@Blob{..} = catching $ withSomeTerm renderToSymbols <$> doParse symbolsToSummarize blob
      where
        catching m = m `catchError` (\(SomeException e) -> pure $ errorFile (show e))
        blobLanguage' = blobLanguage blob
        blobPath' = pack $ blobPath blob
        errorFile e = File blobPath' (bridging # blobLanguage') mempty (V.fromList [ParseError (T.pack e)]) blobOid

        renderToSymbols :: Precise.ToTags t => t Loc -> File
        renderToSymbols term = tagsToFile (Precise.tags blobSource term)

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


data ALaCarteTerm syntax ann = ALaCarteTerm Language [Text] (Term syntax ann)

instance IsTaggable syntax => Precise.ToTags (ALaCarteTerm syntax) where
  tags source (ALaCarteTerm lang symbolsToSummarize term) = runTagging lang source symbolsToSummarize term


data SomeTerm c ann where
  SomeTerm :: c t => t ann -> SomeTerm c ann

withSomeTerm :: (forall t . c t => t ann -> a) -> SomeTerm c ann -> a
withSomeTerm with (SomeTerm term) = with term

doParse :: ParseEffects sig m => [Text] -> Blob -> m (SomeTerm Precise.ToTags Loc)
doParse symbolsToSummarize blob = do
  modes <- ask @PerLanguageModes
  case blobLanguage blob of
    Go         -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.goParser blob
    Haskell    -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.haskellParser blob
    JavaScript -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.tsxParser blob
    JSON       -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.jsonParser blob
    JSX        -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.tsxParser blob
    Markdown   -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.markdownParser blob
    Python
      | Precise <- pythonMode modes -> SomeTerm <$> parse Parser.precisePythonParser blob
      | otherwise                   -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.pythonParser blob
    Ruby       -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.rubyParser blob
    TypeScript -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.typescriptParser blob
    TSX        -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.tsxParser blob
    PHP        -> SomeTerm . ALaCarteTerm (blobLanguage blob) symbolsToSummarize <$> parse Parser.phpParser blob
    _          -> noLanguageForBlob (blobPath blob)

type ParseEffects sig m = (Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Member (Reader Config) sig, Carrier sig m)
