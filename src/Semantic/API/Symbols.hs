{-# LANGUAGE GADTs, TypeOperators, DerivingStrategies #-}
module Semantic.API.Symbols (parseToSymbols, parseToSymbols') where

import           Control.Effect
import           Control.Effect.Error
import           Control.Exception
import           Data.Blob
import           Data.ByteString.Builder
import           Data.Location
import           Data.Maybe
import           Data.Term
import qualified Data.Text as T
import           Parsing.Parser
import           Semantic.API.Converters
import           Semantic.API.Parse
import           Semantic.API.Types
import           Semantic.Task as Task
import           Serializing.Format
import           Tags.Taggable
import           Tags.Tagging

parseToSymbols' :: (Member Distribute sig, ParseEffects sig m, Traversable t)
  => Format ParseTreeSymbolResponse -> t Blob -> m Builder
parseToSymbols' format blobs = runSerialize Plain format <$> parseToSymbols blobs

parseToSymbols :: (Member Distribute sig, ParseEffects sig m, Traversable t) => t Blob -> m ParseTreeSymbolResponse
parseToSymbols blobs = ParseTreeSymbolResponse <$> distributeFoldMap go blobs
  where
    go :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m) => Blob -> m [File]
    go blob@Blob{..} = doParse blobLanguage blob render `catchError` (\(SomeException _) -> pure (pure emptyFile))
      where emptyFile = File (T.pack blobPath) (T.pack (show blobLanguage)) []

    render :: Blob -> SomeTerm TermConstraints Location -> [File]
    render blob (SomeTerm term) = renderToSymbols blob term

    renderToSymbols :: (IsTaggable f) => Blob -> Term f Location -> [File]
    renderToSymbols blob term = either mempty (pure . tagsToFile blob) (runTagging blob term)

    tagsToFile :: Blob -> [Tag] -> File
    tagsToFile Blob{..} tags = File (T.pack blobPath) (T.pack (show blobLanguage)) (fmap tagToSymbol tags)

    tagToSymbol :: Tag -> Symbol
    tagToSymbol Tag{..}
      = Symbol
      { symbolName = name
      , symbolKind = kind
      , symbolLine = fromMaybe mempty line
      , symbolSpan = spanToSpan span
      }
