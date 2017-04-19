{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module SpecHelpers
( diffPaths
, parseFile
, readFileToUnicode
, parserForFilePath
) where

import Data.Functor.Both
import Data.Record
import Info
import Language
import Parser
import Parser.Language hiding (parserForFilePath)
import Prologue
import qualified Data.ByteString as B
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Renderer.SExpression
import Source
import Syntax
import Interpreter
import Data.RandomWalkSimilarity
import Term
import Diff
import System.FilePath
import Patch

-- TODO: Write helper functions for parse file and diff files that don't depend on Command.

-- testDiff :: Both SourceBlob -> IO (Diff (Syntax Text) (Record DefaultFields))

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffPaths :: Both FilePath -> IO ByteString
diffPaths paths = do
  blobs <- traverse readFileToSourceBlob paths
  terms' <- traverse (traverse parseBlob) blobs
  let diff = runDiff terms'
  pure $ renderer (fromMaybe . emptySourceBlob <$> paths <*> blobs) diff
  where
    renderer = sExpression TreeOnly
    runDiff :: HasField fields Category => Both (Maybe (Term (Syntax Text) (Record fields))) -> Diff (Syntax Text) (Record fields)
    runDiff terms = case runJoin terms of
      (Just left, Nothing) -> pure $ Delete left
      (Nothing, Just right) -> pure $ Insert right
      (Just left, Just right) -> stripDiff (runBothWith diffTerms (fmap decorate (both left right)))
      _ -> error "nothing to diff"
      where
        decorate = defaultFeatureVectorDecorator getLabel
        getLabel :: HasField fields Category => TermF (Syntax Text) (Record fields) a -> (Category, Maybe Text)
        getLabel (h :< t) = (Info.category h, case t of
          Leaf s -> Just s
          _ -> Nothing)

-- | Returns an s-expression parse tree for the specified FilePath.
parseFile :: FilePath -> IO ByteString
parseFile path = do
  source <- readFileToUnicode path
  term <- parseBlob $ sourceBlob source path
  pure $ printTerm term 0 TreeOnly

parseBlob :: SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
parseBlob blob@SourceBlob{..} = parserForFilePath path blob

-- | Read a file to a SourceBlob
readFileToSourceBlob :: FilePath -> IO (Maybe SourceBlob)
readFileToSourceBlob path = do
  source <- (Just <$> readFileToUnicode path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe Source))
  pure $ flip sourceBlob path <$> source

-- | Read a file, convert it's contents unicode and return it wrapped in Source.
readFileToUnicode :: FilePath -> IO Source
readFileToUnicode path = B.readFile path >>= transcode
  where
    -- | Transcode a file to a unicode source.
    transcode :: B.ByteString -> IO Source
    transcode text = fromText <$> do
      match <- Detect.detectCharset text
      converter <- Convert.open match Nothing
      pure $ Convert.toUnicode converter text

-- | Return a parser based on the FilePath's extension (including the "."). |
-- NB: This is intentionally duplicated from Parser.Language because our tests
-- will always need to be able to select language from file extention whereas
-- the semantic project should eventually depend on exernal language detection.
parserForFilePath :: FilePath -> Parser (Syntax Text) (Record DefaultFields)
parserForFilePath = parserForLanguage . languageForType . toS . takeExtension
