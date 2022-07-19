{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelpers
( module X
, runBuilder
, parseFilePath
, readFilePathPair
, runTaskOrDie
, runParseWithConfig
, TaskSession(..)
, toList
, Config
, LogQueue
, StatQueue
) where

import qualified Analysis.File as File
import           Analysis.Name as X hiding (Level)
import           Analysis.Project as X
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Lift
import           Control.Carrier.Parse.Simple
import           Control.Carrier.Reader as X
import           Control.Exception (displayException)
import           Control.Monad as X
import           Data.Blob as X
import           Data.Blob.IO as X
import           Data.ByteString as X (ByteString)
import           Data.ByteString.Builder (Builder, toLazyByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Edit as X
import           Data.Foldable (toList)
import           Data.List.NonEmpty as X (NonEmpty (..))
import           Data.Maybe as X
import           Data.Monoid as X (First (..), Last (..), Monoid (..))
import           Data.Proxy as X
import           Data.Semigroup as X (Semigroup (..))
import           Data.Semilattice.Lower as X
import           Data.Traversable as X (for)
import           Debug.Trace as X (traceM, traceShowM)
import           Parsing.Parser as X
import           Semantic.Api hiding (Blob, File)
import           Semantic.Config (Config (..), optionsLogLevel)
import           Semantic.Task as X
import           Semantic.Telemetry (LogQueue, StatQueue)
import           Semantic.Util as X
import           Source.Language as X
import           Source.Range as X hiding (end, point, start)
import           Source.Source as X (Source)
import           Source.Span as X hiding (HasSpan (..), end, point, start)
import qualified Source.Span
import           System.Exit (die)
import           Test.Hspec as X (Spec, SpecWith, around, context, describe, it, parallel, pendingWith, runIO, xit)
import           Test.Hspec.Expectations as X

instance Lower X.Span where
  lowerBound = Source.Span.point (Pos 1 1)

runBuilder :: Builder -> ByteString
runBuilder = toStrict . toLazyByteString

-- | Returns an s-expression parse tree for the specified path.
parseFilePath :: TaskSession -> FilePath -> IO (Either SomeException ByteString)
parseFilePath session path = do
  blob <- readBlobFromFile (File.fromPath path)
  res <- runTask session . runParse (configTreeSitterParseTimeout (config session)) $ parseTermBuilder TermSExpression (toList blob)
  pure (runBuilder <$> res)

runParseWithConfig :: Has (Reader Config) sig m => ParseC m a -> m a
runParseWithConfig task = asks configTreeSitterParseTimeout >>= \ timeout -> runParse timeout task

-- | Read two files to a BlobPair.
readFilePathPair :: FilePath -> FilePath -> IO BlobPair
readFilePathPair p1 p2 = readFilePair (File.fromPath p1) (File.fromPath p2)

-- Run a Task and call `die` if it returns an Exception.
runTaskOrDie :: ParseC TaskC a -> IO a
runTaskOrDie task = runTaskWithOptions defaultOptions { optionsLogLevel = Nothing } (runParseWithConfig task) >>= either (die . displayException) pure
