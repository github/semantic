{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelpers
( module X
, runBuilder
, diffFilePaths
, parseFilePath
, readFilePair
, testEvaluating
, verbatim
, Verbatim(..)
, toList
, Config
, LogQueue
, StatQueue
, lookupDeclaration
) where

import Control.Abstract hiding (lookupDeclaration)
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import qualified Data.Abstract.Heap as Heap
import Control.Arrow ((&&&))
import Control.Effect.Trace as X (runTraceByIgnoring, runTraceByReturning)
import Control.Monad ((>=>))
import Data.Abstract.Address.Precise as X
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.FreeVariables as X
import Data.Abstract.Heap as X
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
import Parsing.Parser as X
import Rendering.Renderer as X hiding (error)
import Semantic.Diff as X
import Semantic.Parse as X
import Semantic.Task as X hiding (parsePackage)
import Semantic.Util as X
import System.FilePath as X

import Data.ByteString as X (ByteString)
import Data.Functor.Both as X (Both, runBothWith, both)
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
import qualified Semantic.IO as IO
import Semantic.Config (Config)
import Semantic.Telemetry (LogQueue, StatQueue)
import System.Exit (die)
import Control.Exception (displayException)

runBuilder = toStrict . toLazyByteString

-- | This orphan instance is so we don't have to insert @name@ calls
-- in dozens and dozens of environment specs.
instance IsString Name where
  fromString = X.name . fromString

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: TaskConfig -> Both FilePath -> IO ByteString
diffFilePaths (TaskConfig config logger statter) paths = readFilePair paths >>= runTaskWithConfig config logger statter . runDiff SExpressionDiffRenderer . pure >>= either (die . displayException) (pure . runBuilder)

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: TaskConfig -> FilePath -> IO ByteString
parseFilePath (TaskConfig config logger statter) path = (fromJust <$> readBlobFromFile (file path)) >>= runTaskWithConfig config logger statter . runParse SExpressionTermRenderer . pure >>= either (die . displayException) (pure . runBuilder)

-- | Read two files to a BlobPair.
readFilePair :: Both FilePath -> IO BlobPair
readFilePair paths = let paths' = fmap file paths in
                     runBothWith F.readFilePair paths'

type TestEvaluatingC term
  = ResumableC (BaseError (AddressError Precise (Val term))) (Eff
  ( ResumableC (BaseError (ValueError term Precise)) (Eff
  ( ResumableC (BaseError ResolutionError) (Eff
  ( ResumableC (BaseError EvalError) (Eff
  ( ResumableC (BaseError (HeapError Precise)) (Eff
  ( ResumableC (BaseError (ScopeError Precise)) (Eff
  ( ResumableC (BaseError (UnspecializedError (Val term))) (Eff
  ( ResumableC (BaseError (LoadError Precise (Val term))) (Eff
  ( FreshC (Eff
  ( TraceByReturningC (Eff
  ( LiftC IO))))))))))))))))))))
type TestEvaluatingErrors term
  = '[ BaseError (AddressError Precise (Val term))
     , BaseError (ValueError term Precise)
     , BaseError ResolutionError
     , BaseError EvalError
     , BaseError (HeapError Precise)
     , BaseError (ScopeError Precise)
     , BaseError (UnspecializedError (Val term))
     , BaseError (LoadError Precise (Val term))
     ]
testEvaluating :: Evaluator term Precise (Val term) (TestEvaluatingC term) (Span, a)
               -> IO
                 ( [String]
                 , Either (SomeError (Data.Sum.Sum (TestEvaluatingErrors term))) a
                 )
testEvaluating
  = runM
  . runTraceByReturning
  . runFresh
  . runEvaluator
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runScopeError
  . runHeapError
  . runEvalError
  . runResolutionError
  . runValueError
  . runAddressError
  . fmap snd

type Val term = Value term Precise


-- deNamespace :: Heap Precise Precise (Value term Precise)
--             -> Value term Precise
--             -> Maybe (Name, [Name])
-- deNamespace heap ns@(Namespace name _ _) = (,) name . Env.allNames <$> namespaceScope heap ns
-- deNamespace _ _                          = Nothing

-- namespaceScope :: Heap Precise Precise (Value term Precise)
--                -> Value term Precise
--                -> Maybe (Environment Precise)
-- namespaceScope heap ns@(Namespace _ _ _)
--   = either (const Nothing) (snd . snd)
--   . run
--   . runFresh
--   . runEvaluator
--   . runAddressError
--   . raiseHandler (runState heap)
--   . raiseHandler (runState (lowerBound @Span))
--   . raiseHandler (runReader (lowerBound @Span))
--   . raiseHandler (runReader (ModuleInfo "SpecHelper.hs"))
--   . runDeref
--   $ undefined

-- namespaceScope _ _ = Nothing

lookupDeclaration :: Name -> Heap Precise Precise (Value term Precise) -> ScopeGraph Precise -> Maybe (Value term Precise)
lookupDeclaration name heap scopeGraph = do
  path <- ScopeGraph.lookupScopePath name scopeGraph
  frameAddress <- Heap.lookupFrameAddress path heap
  set <- Heap.getSlot (Address frameAddress (Heap.pathPosition path)) heap
  fst <$> Set.minView set

newtype Verbatim = Verbatim ByteString
  deriving (Eq)

instance Show Verbatim where
  show (Verbatim x) = show x

verbatim :: ByteString -> Verbatim
verbatim = Verbatim . stripWhitespace
  where
    stripWhitespace :: ByteString -> ByteString
    stripWhitespace = B.foldl' go B.empty
      where go acc x | x `B.elem` " \t\n" = acc
                     | otherwise = B.snoc acc x
