module SpecHelpers
( module X
, runBuilder
, diffFilePaths
, parseFilePath
, readFilePair
, testEvaluating
, deNamespace
, derefQName
, verbatim
, TermEvaluator(..)
, TestEff(..)
, Verbatim(..)
) where

import Analysis.Abstract.Evaluating
import Analysis.Abstract.Evaluating as X (EvaluatingState(..))
import Control.Abstract
import Control.Arrow ((&&&))
import Control.Monad.Effect.Trace as X (runIgnoringTrace, runReturningTrace)
import Control.Monad ((>=>))
import Data.Abstract.Address as X
import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.FreeVariables as X
import Data.Abstract.Heap as X
import Data.Abstract.ModuleTable as X hiding (lookup)
import Data.Abstract.Name as X
import Data.Abstract.Value (Value(..), ValueError, runValueError)
import Data.Bifunctor (first)
import Data.Blob as X
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Project as X
import Data.Proxy as X
import Data.Functor.Listable as X
import Data.Language as X
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Range as X
import Data.Record as X
import Data.Semilattice.Lower as X
import Data.Source as X
import Data.Span as X
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
import qualified Semantic.IO as IO

runBuilder = toStrict . toLazyByteString

-- | Returns an s-expression formatted diff for the specified FilePath pair.
diffFilePaths :: Both FilePath -> IO ByteString
diffFilePaths paths = readFilePair paths >>= fmap runBuilder . runTask . runDiff SExpressionDiffRenderer . pure

-- | Returns an s-expression parse tree for the specified FilePath.
parseFilePath :: FilePath -> IO ByteString
parseFilePath path = (fromJust <$> IO.readFile (file path)) >>= fmap runBuilder . runTask . runParse SExpressionTermRenderer . pure

-- | Read two files to a BlobPair.
readFilePair :: Both FilePath -> IO BlobPair
readFilePair paths = let paths' = fmap file paths in
                     runBothWith IO.readFilePair paths'

testEvaluating :: TermEvaluator term Precise
                    Val
                    '[ Resumable (ValueError Precise TestEff)
                     , Resumable (AddressError Precise Val)
                     , Resumable EvalError, Resumable (EnvironmentError Precise)
                     , Resumable ResolutionError
                     , Resumable (Unspecialized Val)
                     , Resumable (LoadError Precise)
                     , Fresh
                     , State (Heap Precise Latest Val)
                     , State (ModuleTable (Maybe (Precise, Environment Precise)))
                     , Trace
                     ]
                   [(Precise, Environment Precise)]
               -> ((Either
                      (SomeExc
                         (Data.Sum.Sum
                          '[ ValueError Precise TestEff
                           , AddressError Precise Val
                           , EvalError
                           , EnvironmentError Precise
                           , ResolutionError
                           , Unspecialized Val
                           , LoadError Precise
                           ]))
                      [(Value Precise TestEff, Environment Precise)],
                    EvaluatingState Precise Val),
                   [String])
testEvaluating
  = run
  . runReturningTrace
  . evaluating
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runResolutionError
  . runEnvironmentError
  . runEvalError
  . runAddressError
  . runValueError
  . (>>= traverse deref1)
  . runTermEvaluator @_ @_ @Val

type Val = Value Precise TestEff
newtype TestEff a = TestEff
  { runTestEff :: Eff '[ LoopControl Precise
                       , Return Precise
                       , Env Precise
                       , Allocator Precise Val
                       , Reader ModuleInfo
                       , Modules Precise Val
                       , Reader Span
                       , Reader PackageInfo
                       , Resumable (ValueError Precise TestEff)
                       , Resumable (AddressError Precise Val)
                       , Resumable EvalError
                       , Resumable (EnvironmentError Precise)
                       , Resumable ResolutionError
                       , Resumable (Unspecialized Val)
                       , Resumable (LoadError Precise)
                       , Fresh
                       , State (Heap Precise Latest Val)
                       , State (ModuleTable (Maybe (Precise, Environment Precise)))
                       , Trace
                       ] a
  }

deref1 (ptr, env) = runAllocator $ do
  val <- deref ptr
  pure (val, env)

deNamespace :: Value Precise term -> Maybe (Name, [Name])
deNamespace (Namespace name scope) = Just (name, Env.names scope)
deNamespace _                      = Nothing

namespaceScope :: Value Precise term -> Maybe (Environment Precise)
namespaceScope (Namespace _ scope) = Just scope
namespaceScope _                   = Nothing

derefQName :: Heap Precise (Cell Precise) (Value Precise term) -> NonEmpty Name -> Environment Precise -> Maybe (Value Precise term)
derefQName heap = go
  where go (n1 :| ns) env = Env.lookup n1 env >>= flip heapLookup heap >>= getLast . unLatest >>= case ns of
          []        -> Just
          (n2 : ns) -> namespaceScope >=> go (n2 :| ns)

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
