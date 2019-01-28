{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelpers
( module X
, runBuilder
, diffFilePaths
, parseFilePath
, parseTestFile
, readFilePair
, testEvaluating
, verbatim
, Verbatim(..)
, toList
, Config
, LogQueue
, StatQueue
, lookupDeclaration
, lookupMembers
, EdgeLabel(..)
) where

import Control.Abstract hiding (lookupDeclaration)
import Data.Abstract.ScopeGraph (EdgeLabel(..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Abstract.Heap as Heap
import Control.Arrow ((&&&))
import Control.Effect.Trace as X (runTraceByIgnoring, runTraceByReturning)
import Control.Monad ((>=>))
import Data.Traversable as X (for)
import Data.Abstract.Address.Precise as X
import Data.Abstract.Evaluatable hiding (lookupDeclaration)
import Data.Abstract.FreeVariables as X
import Data.Abstract.Module as X
import Data.Abstract.ModuleTable as X hiding (lookup)
import Data.Abstract.Name as X
import Data.Abstract.Value.Concrete (Value(..), ValueError, runValueError)
import Data.Bifunctor (first)
import Data.Blob as X
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Project as X
import Data.Proxy as X
import qualified Data.File as F
import Data.File as X hiding (readFilePair)
import Data.Foldable (toList)
import Data.Functor.Listable as X
import Data.Language as X
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Range as X
import Data.Semilattice.Lower as X
import Data.Source as X
import Data.Span as X
import Data.String
import Data.Sum
import Data.Term as X
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Parsing.Parser as X
import Semantic.Task as X hiding (parsePackage)
import Semantic.Util as X
import Semantic.Graph (runHeap, runScopeGraph)
import System.FilePath as X
import Debug.Trace as X (traceShowM, traceM)

import Data.ByteString as X (ByteString)
import Data.Functor.Both as X (Both (Both), runBothWith)
import Data.Maybe as X
import Data.Monoid as X (Monoid(..), First(..), Last(..))
import Data.Semigroup as X (Semigroup(..))
import Control.Monad as X

import Test.Hspec as X (Spec, SpecWith, context, describe, it, xit, parallel, pendingWith, around, runIO)
import Test.Hspec.Expectations.Pretty as X
import Test.Hspec.LeanCheck as X
import Test.LeanCheck as X

import qualified Data.ByteString as B
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Semantic.IO as IO
import Semantic.Config (Config)
import Semantic.Telemetry (LogQueue, StatQueue)
import Semantic.API hiding (File, Blob, BlobPair)
import System.Exit (die)
import Control.Exception (displayException)

runBuilder = toStrict . toLazyByteString

-- | This orphan instance is so we don't have to insert @name@ calls
-- in dozens and dozens of environment specs.
instance IsString Name where
  fromString = X.name . fromString

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: TaskConfig -> Both FilePath -> IO ByteString
diffFilePaths (TaskConfig config logger statter) paths = readFilePair paths >>= runTaskWithConfig config logger statter . parseDiffBuilder @[] DiffSExpression . pure >>= either (die . displayException) (pure . runBuilder)

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: TaskConfig -> FilePath -> IO ByteString
parseFilePath (TaskConfig config logger statter) path = (fromJust <$> readBlobFromFile (file path)) >>= runTaskWithConfig config logger statter . parseTermBuilder @[] TermSExpression . pure >>= either (die . displayException) (pure . runBuilder)

-- | Read two files to a BlobPair.
readFilePair :: Both FilePath -> IO BlobPair
readFilePair paths = let paths' = fmap file paths in
                     runBothWith F.readFilePair paths'

parseTestFile :: Parser term -> FilePath -> IO (Blob, term)
parseTestFile parser path = runTask $ do
  blob <- readBlob (file path)
  term <- parse parser blob
  pure (blob, term)

type TestEvaluatingC term
  = ResumableC (BaseError (AddressError Precise (Val term))) (Eff
  ( ResumableC (BaseError (ValueError term Precise)) (Eff
  ( ResumableC (BaseError ResolutionError) (Eff
  ( ResumableC (BaseError (EvalError term Precise (Val term))) (Eff
  ( ResumableC (BaseError (HeapError Precise)) (Eff
  ( ResumableC (BaseError (ScopeError Precise)) (Eff
  ( ResumableC (BaseError (UnspecializedError Precise (Val term))) (Eff
  ( ResumableC (BaseError (LoadError Precise (Val term))) (Eff
  ( StateC (Heap Precise Precise (Val term)) (Eff
  ( StateC (ScopeGraph Precise) (Eff
  ( FreshC (Eff
  ( TraceByIgnoringC (Eff
  ( LiftC IO))))))))))))))))))))))))
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
  toList <$> Heap.getSlot (Slot frameAddress (Heap.pathPosition path)) heap

newtype Verbatim = Verbatim ByteString
  deriving (Eq)

instance Show Verbatim where
  showsPrec _ (Verbatim byteString) = (T.unpack (T.decodeUtf8 byteString) ++)

verbatim :: ByteString -> Verbatim
verbatim = Verbatim . stripWhitespace
  where
    stripWhitespace :: ByteString -> ByteString
    stripWhitespace = B.foldl' go B.empty
      where go acc x | x `B.elem` " \t\n" = acc
                     | otherwise = B.snoc acc x
