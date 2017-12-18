{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module SpecHelpers
( diffFilePaths
, parseFilePath
, readFilePair
, languageForFilePath
) where

import Control.Monad ((<=<))
import Control.Exception
import Data.Blob
import qualified Data.ByteString as B
import Data.Functor.Both
import Data.Language
import Data.Maybe (fromMaybe, fromJust)
import Data.Source
import Rendering.Renderer
import Semantic
import Semantic.Task
import qualified Semantic.IO as IO
import System.FilePath

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: Both FilePath -> IO B.ByteString
diffFilePaths paths = readFilePair paths >>= runTask . diffBlobPair SExpressionDiffRenderer

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: FilePath -> IO B.ByteString
parseFilePath path = IO.readFile path (languageForFilePath path) >>= pure . fromJust >>= runTask . parseBlob SExpressionTermRenderer

-- | Read two files to a BlobPair.
readFilePair :: Both FilePath -> IO BlobPair
readFilePair paths = let paths' = fmap (\p -> (p, languageForFilePath p)) paths in
                     runBothWith IO.readFilePair paths'

-- | Returns a Maybe Language based on the FilePath's extension.
languageForFilePath :: FilePath -> Maybe Language
languageForFilePath = languageForType . takeExtension
