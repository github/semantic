{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving, MonoLocalBinds, RankNTypes, StandaloneDeriving #-}
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
import           Data.Abstract.Declarations (Declarations1)
import           Data.Blob hiding (File (..))
import           Data.ByteString.Builder
import           Data.Language
import           Data.ProtoLens (defMessage)
import           Data.Term
import           Data.Text (pack)
import qualified Language.Java as Java
import qualified Language.JSON as JSON
import qualified Language.Go.Term as Go
import qualified Language.Markdown.Term as Markdown
import qualified Language.PHP.Term as PHP
import qualified Language.Python as PythonPrecise
import qualified Language.Python.Term as PythonALaCarte
import qualified Language.Ruby.Term as Ruby
import qualified Language.TSX.Term as TSX
import qualified Language.TypeScript.Term as TypeScript
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
import           Source.Source
import           Tags.Taggable
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
        renderToSymbols = pure . tagsToFile . tags (blobLanguage blob) symbolsToSummarize blobSource

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
    go blob@Blob{..} = catching $ asks toTagsParsers >>= \ p -> parseWith p (pure . renderToSymbols) blob
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

        renderToSymbols :: ToTags t => t Loc -> File
        renderToSymbols term = tagsToFile (tags (blobLanguage blob) symbolsToSummarize blobSource term)

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

symbolsToSummarize :: [Text]
symbolsToSummarize = ["Function", "Method", "Class", "Module", "Call", "Send"]

class ToTags t where
  tags :: Language -> [Text] -> Source -> t Loc -> [Tag]

instance (IsTaggable syntax, Declarations1 syntax) => ToTags (Term syntax) where
  tags = runTagging

deriving instance ToTags Go.Term
deriving instance ToTags Markdown.Term
deriving instance ToTags PHP.Term
deriving instance ToTags PythonALaCarte.Term
instance ToTags Ruby.Term where
  tags = runTagging
instance ToTags TSX.Term where
  tags = runTagging
instance ToTags TypeScript.Term where
  tags = runTagging

deriving via (ViaPrecise Java.Term)          instance ToTags Java.Term
deriving via (ViaPrecise JSON.Term)          instance ToTags JSON.Term
deriving via (ViaPrecise PythonPrecise.Term) instance ToTags PythonPrecise.Term


newtype ViaPrecise t a = ViaPrecise (t a)

instance Precise.ToTags t => ToTags (ViaPrecise t) where
  tags _ _ src (ViaPrecise t) = Precise.tags src t


toTagsParsers :: PerLanguageModes -> Map Language (Parser.SomeParser ToTags Loc)
toTagsParsers = Parser.allParsers
