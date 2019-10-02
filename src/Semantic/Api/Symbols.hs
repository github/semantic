{-# LANGUAGE MonoLocalBinds, RankNTypes #-}
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
import           Data.ProtoLens (defMessage)
import           Data.Term
import           Data.Text (pack)
import qualified Data.Text as T
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
import           Source.Loc
import           Tags.Taggable
import           Tags.Tagging
import qualified Tags.Tagging.Precise as Precise

legacyParseSymbols :: (Member Distribute sig, Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m, Traversable t) => t Blob -> m Legacy.ParseTreeSymbolResponse
legacyParseSymbols blobs = Legacy.ParseTreeSymbolResponse <$> distributeFoldMap go blobs
  where
    go :: (Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m) => Blob -> m [Legacy.File]
    go blob@Blob{..} = doParse (pure . renderToSymbols) symbolsToSummarize blob `catchError` (\(SomeException _) -> pure (pure emptyFile))
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

parseSymbolsBuilder :: (Member Distribute sig, Member (Error SomeException) sig, Member Parse sig, Member (Reader Config) sig, Member (Reader PerLanguageModes) sig, Carrier sig m, Traversable t) => Format ParseTreeSymbolResponse -> t Blob -> m Builder
parseSymbolsBuilder format blobs = parseSymbols blobs >>= serialize format

parseSymbols :: (Member Distribute sig, Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseSymbols blobs = do
  terms <- distributeFor blobs go
  pure $ defMessage & P.files .~ toList terms
  where
    go :: (Member (Error SomeException) sig, Member (Reader PerLanguageModes) sig, Member Parse sig, Carrier sig m) => Blob -> m File
    go blob@Blob{..} = catching $ doParse (pure . renderToSymbols) symbolsToSummarize blob
      where
        catching m = m `catchError` (\(SomeException e) -> pure $ errorFile (show e))
        blobLanguage' = blobLanguage blob
        blobPath' = pack $ blobPath blob
        errorFile e = defMessage
          & P.path .~ blobPath'
          & P.language .~ (bridging # blobLanguage')
          & P.symbols .~ mempty
          & P.errors .~ [defMessage & P.error .~ T.pack e]
          & P.blobOid .~ blobOid

        renderToSymbols :: Precise.ToTags t => t Loc -> File
        renderToSymbols term = tagsToFile (Precise.tags blobSource term)

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
          & P.maybe'span .~ converting #? span
          & P.maybe'docs .~ fmap (flip (set P.docstring) defMessage) docs

        symbolsToSummarize :: [Text]
        symbolsToSummarize = ["Function", "Method", "Class", "Module", "Call", "Send"]

data ALaCarteTerm syntax ann = ALaCarteTerm Language [Text] (Term syntax ann)

instance IsTaggable syntax => Precise.ToTags (ALaCarteTerm syntax) where
  tags source (ALaCarteTerm lang symbolsToSummarize term) = runTagging lang source symbolsToSummarize term


doParse
  :: ( Carrier sig m
     , Member (Error SomeException) sig
     , Member Parse sig
     , Member (Reader PerLanguageModes) sig
     )
  => (forall t . Precise.ToTags t => t Loc -> m a)
  -> [Text]
  -> Blob
  -> m a
doParse with symbolsToSummarize blob = do
  modes <- ask @PerLanguageModes
  case blobLanguage blob of
    Go         -> parse Parser.goParser         blob >>= with . mkTerm
    Haskell    -> parse Parser.haskellParser    blob >>= with . mkTerm
    JavaScript -> parse Parser.tsxParser        blob >>= with . mkTerm
    JSON       -> parse Parser.jsonParser       blob >>= with . mkTerm
    JSX        -> parse Parser.tsxParser        blob >>= with . mkTerm
    Markdown   -> parse Parser.markdownParser   blob >>= with . mkTerm
    Python
      | Precise <- pythonMode modes -> parse Parser.precisePythonParser blob >>= with
      | otherwise                   -> parse Parser.pythonParser        blob >>= with . mkTerm
    Ruby       -> parse Parser.rubyParser       blob >>= with . mkTerm
    TypeScript -> parse Parser.typescriptParser blob >>= with . mkTerm
    TSX        -> parse Parser.tsxParser        blob >>= with . mkTerm
    PHP        -> parse Parser.phpParser        blob >>= with . mkTerm
    _          -> noLanguageForBlob (blobPath blob)
    where mkTerm :: Term syntax Loc -> ALaCarteTerm syntax Loc
          mkTerm = ALaCarteTerm (blobLanguage blob) symbolsToSummarize
