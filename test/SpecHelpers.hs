{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module SpecHelpers
( diffFilePaths
, parseFilePath
, readFile
, languageForFilePath
, unListableDiff
) where

import qualified Data.ByteString as B
import Data.Functor.Both
import Data.Functor.Listable
import qualified Data.Text.ICU.Convert as Convert
import qualified Data.Text.ICU.Detect as Detect
import Diff
import Language
import Patch
import Prologue hiding (readFile)
import Renderer
import Semantic
import Semantic.Task
import Source
import System.FilePath
import Term

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: Both FilePath -> IO ByteString
diffFilePaths paths = do
  blobs <- traverse readFile paths
  fromMaybe mempty <$> runTask (parseDiffAndRenderBlobPair SExpressionDiffRenderer blobs)

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: FilePath -> IO ByteString
parseFilePath path = do
  blob <- readFile path
  runTask (parseBlob SExpressionTermRenderer blob)

-- | Read a file to a SourceBlob.
--
-- NB: This is intentionally duplicated from Command.Files because eventually
-- we want to be able to test a core Semantic library that has no knowledge of
-- the filesystem or Git. The tests, however, will still leverage reading files.
readFile :: FilePath -> IO SourceBlob
readFile path = do
  source <- (Just <$> readFileToUnicode path) `catch` (const (pure Nothing) :: IOException -> IO (Maybe Source))
  pure $ fromMaybe (emptySourceBlob path) (sourceBlob path (languageForFilePath path) <$> source)
  where
    -- | Read a file, convert it's contents unicode and return it wrapped in Source.
    readFileToUnicode :: FilePath -> IO Source
    readFileToUnicode path = B.readFile path >>= transcode
      where
        transcode :: B.ByteString -> IO Source
        transcode text = fromText <$> do
          match <- Detect.detectCharset text
          converter <- Convert.open match Nothing
          pure $ Convert.toUnicode converter text

-- | Returns a Maybe Language based on the FilePath's extension.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . toS . takeExtension

-- | Extract a 'Diff' from a 'ListableF' enumerated by a property test.
unListableDiff :: Functor f => ListableF (Free (TermF f (ListableF (Join (,)) annotation))) (Patch (ListableF (Term f) annotation)) -> Diff f annotation
unListableDiff diff = hoistFree (first unListableF) $ fmap unListableF <$> unListableF diff
