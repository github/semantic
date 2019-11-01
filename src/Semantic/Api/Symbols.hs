{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Semantic.Api.Symbols
  ( legacyParseSymbols
  , parseSymbols
  , parseSymbolsBuilder
  , tagsForBlob
  ) where

import           Control.Effect.Error
import           Control.Effect.Parse
import           Control.Effect.Reader
import           Control.Exception
import           Control.Lens
import           Data.Abstract.Declarations
import           Data.Blob hiding (File (..))
import           Data.ByteString.Builder
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Data.Term (IsTerm(..), TermF)
import           Data.Text (pack)
import qualified Parsing.Parser as Parser
import           Prologue
import           Proto.Semantic as P hiding (Blob, BlobPair)
import           Proto.Semantic_Fields as P
import           Proto.Semantic_JSON ()
import           Semantic.Api.Bridge
import qualified Semantic.Api.LegacyTypes as Legacy
import           Semantic.Config
import           Semantic.Task
import           Serializing.Format (Format)
import           Source.Loc as Loc
import           Tags.Tagging
import qualified Tags.Tagging.Precise as Precise

legacyParseSymbols :: (Member Distribute sig, Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m, Traversable t) => t Blob -> m Legacy.ParseTreeSymbolResponse
legacyParseSymbols blobs = Legacy.ParseTreeSymbolResponse <$> distributeFoldMap go blobs
  where
    go :: (Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m) => Blob -> m [Legacy.File]
    go blob@Blob{..} = asks toTagsParsers >>= \ p -> parseWith p (pure . renderToSymbols) blob `catchError` (\(SomeException _) -> pure (pure emptyFile))
      where
        emptyFile = tagsToFile []

        -- Legacy symbols output doesn't include Function Calls.
        symbolsToSummarize :: [Text]
        symbolsToSummarize = ["Function", "Method", "Class", "Module"]

        renderToSymbols :: ToTags t => t Loc -> [Legacy.File]
        renderToSymbols = pure . tagsToFile . tags symbolsToSummarize blob

        tagsToFile :: [Tag] -> Legacy.File
        tagsToFile tags = Legacy.File (pack (blobPath blob)) (pack (show (blobLanguage blob))) (fmap tagToSymbol tags)

        tagToSymbol :: Tag -> Legacy.Symbol
        tagToSymbol Tag{..}
          = Legacy.Symbol
          { symbolName = name
          , symbolKind = pack (show kind)
          , symbolLine = line
          , symbolSpan = converting #? Loc.span loc
          }

parseSymbolsBuilder :: (Member Distribute sig, Member (Error SomeException) sig, Member Parse sig, Member (Reader Config) sig, Member (Reader PerLanguageModes) sig, Carrier sig m, Traversable t) => Format ParseTreeSymbolResponse -> t Blob -> m Builder
parseSymbolsBuilder format blobs = parseSymbols blobs >>= serialize format

parseSymbols :: (Member Distribute sig, Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseSymbols blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList terms
  where
    go :: (Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m) => Blob -> m File
    go blob@Blob{..} = catching $ tagsToFile <$> tagsForBlob blob
      where
        catching m = m `catchError` (\(SomeException e) -> pure $ errorFile (show e))
        blobLanguage' = blobLanguage blob
        blobPath' = pack $ blobPath blob
        errorFile e = defMessage
          & P.path .~ blobPath'
          & P.language .~ (bridging # blobLanguage')
          & P.symbols .~ mempty
          & P.errors .~ [defMessage & P.error .~ pack e]
          & P.blobOid .~ blobOid

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
          & P.kind .~ pack (show kind)
          & P.line .~ line
          & P.maybe'span ?~ converting # Loc.span loc
          & P.maybe'docs .~ fmap (flip (set P.docstring) defMessage) docs

tagsForBlob :: (Carrier sig m, Member (Error SomeException) sig, Member Parse sig, Member (Reader PerLanguageModes) sig) => Blob -> m [Tag]
tagsForBlob blob = asks toTagsParsers >>= \p -> parseWith p (pure . tags symbolsToSummarize blob) blob

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module", "Call", "Send"]

class ToTags t where
  tags :: [Text] -> Blob -> t Loc -> [Tag]

instance (Parser.TermMode term ~ strategy, ToTagsBy strategy term) => ToTags term where
  tags = tagsBy @strategy

class ToTagsBy (strategy :: LanguageMode) term where
  tagsBy :: [Text] -> Blob -> term Loc -> [Tag]

instance (IsTerm term, IsTaggable (Syntax term), Base (term Loc) ~ TermF (Syntax term) Loc, Recursive (term Loc), Declarations (term Loc)) => ToTagsBy 'ALaCarte term where
  tagsBy symbols blob = runTagging (blobLanguage blob) symbols (blobSource blob)

instance Precise.ToTags term => ToTagsBy 'Precise term where
  tagsBy _ = Precise.tags . blobSource


toTagsParsers :: PerLanguageModes -> Map Language (Parser.SomeParser ToTags Loc)
toTagsParsers = Parser.allParsers
