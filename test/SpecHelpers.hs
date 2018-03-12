{-# LANGUAGE DataKinds, GADTs, ScopedTypeVariables, TypeFamilies, TypeOperators, TypeApplications #-}
module SpecHelpers
( file
, evaluateFiles
, diffFilePaths
, parseFilePath
, readFilePair
, Verbatim(..)
, verbatim
, readFileVerbatim
) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class
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
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Analysis.Abstract.Evaluating
import Data.Map as Map
import Data.Union
import Data.Semigroup
import Data.Functor.Foldable
import Data.Abstract.Evaluatable
import Data.Abstract.Address
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.ModuleTable
import Data.Abstract.Store
import Data.Abstract.Value as Value
import Parsing.Parser

file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (IO.languageForFilePath path)

evaluateFiles :: forall v term.
              ( Data.Abstract.Evaluatable.Evaluatable (Data.Functor.Foldable.Base term)
              , FreeVariables term
              , MonadAddressable (LocationFor v) v (Evaluation term v)
              , MonadValue term v (Evaluation term v)
              , Ord (LocationFor v)
              , Recursive term
              , Semigroup (Cell (LocationFor v) v)
              )
              => Parser term
              -> [FilePath]
              -> IO (
                     (
                       (
                         ( Either Prelude.String v
                         , Store (LocationFor v) v
                         )
                         , Map.Map Data.Abstract.FreeVariables.Name (Data.Abstract.FreeVariables.Name, Maybe (Address (LocationFor v) v))
                       )
                       , Environment (LocationFor v) v
                     )
                     , ModuleTable (Environment (LocationFor v) v)
                    )
evaluateFiles parser paths = do
  blobs@(b:bs) <- traverse file paths
  (t:ts) <- runTask $ traverse (parse parser) blobs
  pure $ evaluates @v (zip bs ts) (b, t)

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
