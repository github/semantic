{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module SpecHelpers
( diffFilePaths
, parseFilePath
, readFile
, readFilePair
, languageForFilePath
) where

import Control.Monad ((<=<))
import Control.Exception
import Data.Blob
import qualified Data.ByteString as B
import Data.Functor.Both
import Data.Language
import Data.Maybe (fromMaybe)
import Data.Source
import Prelude hiding (readFile)
import Rendering.Renderer
import Semantic
import Semantic.Task
import qualified Semantic.IO as IO
import System.FilePath

readFilePair :: Both FilePath -> IO BlobPair
readFilePair paths = do
  let paths' = fmap (\p -> (p, languageForFilePath p)) paths
  runBothWith IO.readFilePair paths'

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: Both FilePath -> IO B.ByteString
diffFilePaths paths = readFilePair paths >>= runTask . diffBlobPair SExpressionDiffRenderer

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: FilePath -> IO B.ByteString
parseFilePath path = do
  blob <- readFile path
  runTask (parseBlob SExpressionTermRenderer blob)

-- | Read a file to a Blob.
--
-- NB: This is intentionally duplicated from Command.Files because eventually
-- we want to be able to test a core Semantic library that has no knowledge of
-- the filesystem or Git. The tests, however, will still leverage reading files.
readFile :: FilePath -> IO Blob
readFile path = do
  source <- (Just . fromBytes <$> B.readFile path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe Source))
  pure $ fromMaybe (emptyBlob path) (sourceBlob path (languageForFilePath path) <$> source)

-- | Returns a Maybe Language based on the FilePath's extension.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . takeExtension
