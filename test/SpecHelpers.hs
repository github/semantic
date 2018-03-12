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

parseFile :: Parser term -> FilePath -> IO (Blob, term)
parseFile parser path = runTask $ do
  blob <- file path
  (,) blob <$> parse parser blob

evaluateFiles :: forall value term
              .  ( Evaluatable (Base term)
                 , FreeVariables term
                 , MonadAddressable (LocationFor value) value (Evaluating term value (EvaluatingEffects term value))
                 , MonadValue term value (Evaluating term value (EvaluatingEffects term value))
                 , Recursive term
                 )
              => Parser term
              -> [FilePath]
              -> IO (
                     (
                       (
                         ( Either Prelude.String value
                         , Environment (LocationFor value) value
                         )
                         , Store (LocationFor value) value
                       )
                       , ModuleTable (Environment (LocationFor value) value)
                     )
                     , Map.Map Data.Abstract.FreeVariables.Name (Data.Abstract.FreeVariables.Name, Maybe (Address (LocationFor value) value))
                    )
evaluateFiles parser paths = do
  entry:xs <- traverse (parseFile parser) paths
  pure $ evaluates @value xs entry

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
