{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelpers
( module X
, runBuilder
, diffFilePaths
, parseFilePath
, parseTestFile
, readFilePathPair
, runTaskOrDie
, TaskSession(..)
, testEvaluating
, toList
, Config
, LogQueue
, StatQueue
, lookupDeclaration
, lookupMembers
, EdgeLabel(..)
) where

import Control.Abstract
import Data.Abstract.ScopeGraph (EdgeLabel(..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Abstract.Heap as Heap
import Control.Effect.Lift
import Control.Effect.Trace as X (runTraceByIgnoring, runTraceByReturning)
import Control.Monad ((>=>))
import Data.Traversable as X (for)
import Data.Abstract.Address.Precise as X
import Data.Abstract.Evaluatable
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module as X
import Data.Abstract.ModuleTable as X hiding (lookup)
import Data.Abstract.Name as X
import Data.Abstract.Value.Concrete (Value(..), ValueError, runValueError)
import Data.Blob as X
import Data.Blob.IO as X
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Project as X
import Data.Proxy as X
import Data.Foldable (toList)
import Data.Functor.Listable as X
import Data.Language as X
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Semilattice.Lower as X
import Data.Source as X
import Data.String
import Data.Sum
import Data.Term as X
import Parsing.Parser as X
import Semantic.Task as X
import Semantic.Util as X
import Semantic.Graph (runHeap, runScopeGraph)
import Source.Range as X
import Source.Span as X hiding (HasSpan(..))
import System.FilePath as X
import Debug.Trace as X (traceShowM, traceM)

import Data.ByteString as X (ByteString)
import Data.Functor.Both as X (Both (Both), runBothWith)
import Data.Maybe as X
import Data.Monoid as X (Monoid(..), First(..), Last(..))
import Data.Semigroup as X (Semigroup(..))
import Control.Monad as X

import Test.Hspec as X (Spec, SpecWith, context, describe, it, xit, parallel, pendingWith, around, runIO)
import Test.Hspec.Expectations as X
import Test.Hspec.LeanCheck as X
import Test.LeanCheck as X

import Semantic.Config (Config(..), optionsLogLevel)
import Semantic.Telemetry (LogQueue, StatQueue)
import Semantic.Api hiding (File, Blob, BlobPair)
import System.Exit (die)
import Control.Exception (displayException)

runBuilder :: Builder -> ByteString
runBuilder = toStrict . toLazyByteString

-- | This orphan instance is so we don't have to insert @name@ calls
-- in dozens and dozens of environment specs.
instance IsString Name where
  fromString = X.name . fromString

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: TaskSession -> Both FilePath -> IO ByteString
diffFilePaths session paths = readFilePathPair paths >>= runTask session . parseDiffBuilder @[] DiffSExpression . pure >>= either (die . displayException) (pure . runBuilder)

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: TaskSession -> FilePath -> IO (Either SomeException ByteString)
parseFilePath session path = do
  blob <- readBlobFromFile (fileForPath path)
  res <- runTask session $ parseTermBuilder TermSExpression (toList blob)
  pure (runBuilder <$> res)

-- | Read two files to a BlobPair.
readFilePathPair :: Both FilePath -> IO BlobPair
readFilePathPair paths = let paths' = fmap fileForPath paths in
                     runBothWith readFilePair paths'

parseTestFile :: Parser term -> FilePath -> IO (Blob, term)
parseTestFile parser path = runTaskOrDie $ do
  blob <- readBlob (fileForPath path)
  term <- parse parser blob
  pure (blob, term)

-- Run a Task and call `die` if it returns an Exception.
runTaskOrDie :: TaskEff a -> IO a
runTaskOrDie task = runTaskWithOptions defaultOptions { optionsLogLevel = Nothing } task >>= either (die . displayException) pure

type TestEvaluatingC term
  = ResumableC (BaseError (AddressError Precise (Val term)))
  ( ResumableC (BaseError (ValueError term Precise))
  ( ResumableC (BaseError ResolutionError)
  ( ResumableC (BaseError (EvalError term Precise (Val term)))
  ( ResumableC (BaseError (HeapError Precise))
  ( ResumableC (BaseError (ScopeError Precise))
  ( ResumableC (BaseError (UnspecializedError Precise (Val term)))
  ( ResumableC (BaseError (LoadError Precise (Val term)))
  ( StateC (Heap Precise Precise (Val term))
  ( StateC (ScopeGraph Precise)
  ( FreshC
  ( TraceByIgnoringC
  ( LiftC IO))))))))))))
type TestEvaluatingErrors term
  = '[ BaseError (AddressError Precise (Val term))
     , BaseError (ValueError term Precise)
     , BaseError ResolutionError
     , BaseError (EvalError term Precise (Val term))
     , BaseError (HeapError Precise)
     , BaseError (ScopeError Precise)
     , BaseError (UnspecializedError Precise (Val term))
     , BaseError (LoadError Precise (Val term))
     ]
testEvaluating :: Evaluator term Precise (Val term) (TestEvaluatingC term) a
               -> IO
                  (ScopeGraph Precise,
                    (Heap Precise Precise (Value term Precise),
                     Either (SomeError (Data.Sum.Sum (TestEvaluatingErrors term))) a))
testEvaluating
  = runM
  . runTraceByIgnoring
  . runFresh
  . runEvaluator
  . runScopeGraph
  . runHeap
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runScopeError
  . runHeapError
  . runEvalError
  . runResolutionError
  . runValueError
  . runAddressError

type Val term = Value term Precise


members :: EdgeLabel
        -> Heap Precise Precise (Value term Precise)
        -> ScopeGraph Precise
        -> Value term Precise
        -> Maybe [Name]
members edgeLabel heap scopeGraph (Data.Abstract.Value.Concrete.Object frame)    = frameNames [ edgeLabel ] heap scopeGraph frame
members edgeLabel heap scopeGraph (Class _ _ frame) = frameNames [ edgeLabel ] heap scopeGraph frame
members _ _ _ _                                     = Nothing

frameNames :: [ EdgeLabel ]
           -> Heap Precise Precise (Value term Precise)
           -> ScopeGraph Precise
           -> Precise
           -> Maybe [ Name ]
frameNames edge heap scopeGraph frame = do
  scopeAddress <- Heap.scopeLookup frame heap
  scope <- ScopeGraph.lookupScope scopeAddress scopeGraph
  pure (unDeclaration <$> toList (ScopeGraph.declarationNames edge scope scopeGraph))

lookupMembers :: Name -> EdgeLabel -> (Precise, Precise) -> Heap Precise Precise (Value term Precise) -> ScopeGraph Precise -> Maybe [ Name ]
lookupMembers name edgeLabel scopeAndFrame heap scopeGraph =
  (lookupDeclaration name scopeAndFrame heap scopeGraph >>= members edgeLabel heap scopeGraph . Prelude.head)

lookupDeclaration :: Name -> (Precise, Precise) -> Heap Precise Precise (Value term Precise) -> ScopeGraph Precise -> Maybe [ Value term Precise ]
lookupDeclaration name (currentScope, currentFrame) heap scopeGraph = do
  path <- ScopeGraph.lookupScopePath name currentScope scopeGraph
  frameAddress <- Heap.lookupFrameAddress path currentFrame heap
  toList <$> Heap.getSlotValue (Slot frameAddress (Heap.pathPosition path)) heap
