{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators, TypeApplications #-}
module SpecHelpers (
  module X
, diffFilePaths
, parseFilePath
, readFilePair
, Verbatim(..)
, verbatim
, readFileVerbatim
, ) where

import Analysis.Abstract.Evaluating
import Control.Exception
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Abstract.Address as X
import Data.Abstract.Environment as X
import Data.Abstract.Evaluatable as X
import Data.Abstract.FreeVariables as X
import Data.Abstract.ModuleTable as X
import Data.Abstract.Store as X
-- import Data.Abstract.Value as Value
import Data.Blob
import Data.Functor.Both
import Data.Functor.Foldable
import Data.Language
import Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Semigroup
import Data.Source
import Data.Text.Encoding (decodeUtf8)
-- import Data.Union
import Parsing.Parser as X
import qualified Data.ByteString as B
-- import qualified Data.Text as T
import qualified Semantic.IO as IO
import Rendering.Renderer
import Semantic
import Semantic.Task
import Semantic.Util as X
-- import System.FilePath
import Data.Monoid as X (Monoid(..), First(..), Last(..))
import Data.Semigroup as X (Semigroup(..))

import Test.Hspec as X (Spec, describe, it, xit, parallel, pendingWith)
import Test.Hspec.Expectations.Pretty as X

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: Both FilePath -> IO B.ByteString
diffFilePaths paths = readFilePair paths >>= runTask . diffBlobPair SExpressionDiffRenderer

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: FilePath -> IO B.ByteString
parseFilePath path = IO.readFile path (IO.languageForFilePath path) >>= pure . fromJust >>= runTask . parseBlob SExpressionTermRenderer

-- | Read two files to a BlobPair.
readFilePair :: Both FilePath -> IO BlobPair
readFilePair paths = let paths' = fmap (\p -> (p, IO.languageForFilePath p)) paths in
                     runBothWith IO.readFilePair paths'

readFileVerbatim :: FilePath -> IO Verbatim
readFileVerbatim = fmap verbatim . B.readFile

newtype Verbatim = Verbatim B.ByteString
  deriving (Eq)

instance Show Verbatim where
  show (Verbatim x) = show x

verbatim :: B.ByteString -> Verbatim
verbatim = Verbatim . stripWhitespace
  where
    stripWhitespace :: B.ByteString -> B.ByteString
    stripWhitespace = B.foldl' go B.empty
      where go acc x | x `B.elem` " \t\n" = acc
                     | otherwise = B.snoc acc x
