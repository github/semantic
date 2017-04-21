{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module SpecHelpers
( diffFilePaths
, parseFilePath
, readFileToUnicode
, parserForFilePath
) where

import Data.Functor.Both
import Data.Record
import Info
import Language
import Parser
import Parser.Language hiding (parserForFilePath)
import Prologue hiding (readFile)
import qualified Data.ByteString as B
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Renderer
import Renderer.SExpression
import Semantic
import Source
import Syntax
import System.FilePath

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: Both FilePath -> IO ByteString
diffFilePaths paths = do
  blobs <- pure <$> traverse readFile paths
  diffBlobs (SExpressionDiffRenderer TreeOnly) blobs

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: FilePath -> IO ByteString
parseFilePath path = do
  source <- readFileToUnicode path
  parseBlobs (SExpressionParseTreeRenderer TreeOnly) [sourceBlob source path]

-- | Read a file, convert it's contents unicode and return it wrapped in Source.
readFileToUnicode :: FilePath -> IO Source
readFileToUnicode path = B.readFile path >>= transcode
  where
    transcode :: B.ByteString -> IO Source
    transcode text = fromText <$> do
      match <- Detect.detectCharset text
      converter <- Convert.open match Nothing
      pure $ Convert.toUnicode converter text

-- | Return a parser based on the FilePath's extension (including the ".").
--
-- NB: This is intentionally duplicated from Parser.Language because our tests
-- will always need to be able to select language from file extention whereas
-- the semantic project should eventually depend on exernal language detection.
parserForFilePath :: FilePath -> Parser (Syntax Text) (Record DefaultFields)
parserForFilePath = parserForLanguage . languageForType . toS . takeExtension

-- | Read a file to a SourceBlob.
--
-- NB: This is intentionally duplicated from Command.Files because eventually
-- we want to be able to test a core Semantic library that has no knowlege of
-- the filesystem or Git. The tests, however, will still leverage reading files.
readFile :: FilePath -> IO SourceBlob
readFile path = do
  source <- (Just <$> readFileToUnicode path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe Source))
  pure $ fromMaybe (emptySourceBlob path) (flip sourceBlob path <$> source)
