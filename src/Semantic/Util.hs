-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import Analysis.Abstract.BadVariables
import Analysis.Abstract.Caching
import Analysis.Abstract.Dead
import Analysis.Abstract.Evaluating as X
import Analysis.Abstract.ImportGraph
import Analysis.Abstract.Tracing
import Analysis.Declaration
import Control.Abstract.Analysis
import Control.Monad.IO.Class
import Data.Abstract.Evaluatable hiding (head)
import Data.Abstract.Address
import Data.Abstract.Located
import Data.Abstract.Module
import Data.Abstract.Package as Package
import Data.Abstract.Type
import Data.Abstract.Value
import Data.Blob
import Data.Diff
import Data.Range
import Data.Record
import Data.Semigroup.Reducer
import Data.Span
import Data.Term
import Diffing.Algorithm
import Diffing.Interpreter
import System.FilePath.Glob
import qualified GHC.TypeLits as TypeLevel
import Language.Preluded
import Parsing.Parser
import Prologue
import Semantic.Diff (diffTermPair)
import Semantic.IO as IO
import Semantic.Task hiding (parsePackage)
import qualified Semantic.Task as Task
import System.FilePath.Posix

import qualified Language.Go.Assignment as Go
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript

-- Ruby
evalRubyProject = evaluateProjectWithPrelude rubyParser ["rb"]
evalRubyFile = evaluateWithPrelude rubyParser
evaluateRubyImportGraph name paths = runAnalysis @(ImportGraphing (Evaluating (Located Precise Ruby.Term) Ruby.Term (Value (Located Precise Ruby.Term)))) . evaluatePackage <$> parsePackage name rubyParser (dropFileName (head paths)) paths
evaluateRubyBadVariables paths = runAnalysis @(BadVariables (Evaluating Precise Ruby.Term (Value Precise))) . evaluateModules <$> parseFiles rubyParser (dropFileName (head paths)) paths

-- Go
evalGoProject = evaluateProject goParser ["go"]
evalGoFile = evaluateFile goParser
typecheckGoFile path = runAnalysis @(Caching (Evaluating Monovariant Go.Term Type)) . evaluateModule <$> parseFile goParser Nothing path

-- Python
evalPythonProject = evaluateProjectWithPrelude pythonParser ["py"]
evalPythonFile = evaluateWithPrelude pythonParser
evaluatePythonImportGraph name paths = runAnalysis @(ImportGraphing (Evaluating (Located Precise Python.Term) Python.Term (Value (Located Precise Python.Term)))) . evaluatePackage <$> parsePackage name pythonParser (dropFileName (head paths)) paths
typecheckPythonFile path = runAnalysis @(Caching (Evaluating Monovariant Python.Term Type)) . evaluateModule <$> parseFile pythonParser Nothing path
tracePythonFile path = runAnalysis @(Tracing [] (Evaluating Precise Python.Term (Value Precise))) . evaluateModule <$> parseFile pythonParser Nothing path
evaluateDeadTracePythonFile path = runAnalysis @(DeadCode (Tracing [] (Evaluating Precise Python.Term (Value Precise)))) . evaluateModule <$> parseFile pythonParser Nothing path

-- PHP
evalPHPProject = evaluateProject phpParser ["php"]
evalPHPFile = evaluateFile phpParser

-- TypeScript
evalTypeScriptProject = evaluateProject typescriptParser ["ts", "tsx"]
evalTypeScriptFile = evaluateFile typescriptParser
typecheckTypeScriptFile path = runAnalysis @(Caching (Evaluating Monovariant TypeScript.Term Type)) . evaluateModule <$> parseFile typescriptParser Nothing path

evaluateProject :: forall term effects
                .  ( Corecursive term
                  , Evaluatable (Base term)
                  , FreeVariables term
                  , effects ~ Effects Precise term (Value Precise) (Evaluating Precise term (Value Precise) effects)
                  , MonadAddressable Precise (Evaluating Precise term (Value Precise) effects)
                  , Recursive term
                  )
                => Parser term
                -> [FilePath]
                -> FilePath
                -> IO (Final effects (Value Precise))
evaluateProject parser exts entryPoint = do
  let rootDir = takeDirectory entryPoint
  paths <- filter (/= entryPoint) <$> getPaths exts rootDir
  evaluateFiles parser rootDir (entryPoint : paths)

evaluateProjectWithPrelude :: forall term effects
                           .  ( Corecursive term
                              , Evaluatable (Base term)
                              , FreeVariables term
                              , effects ~ Effects Precise term (Value Precise) (Evaluating Precise term (Value Precise) effects)
                              , MonadAddressable Precise (Evaluating Precise term (Value Precise) effects)
                              , Recursive term
                              , TypeLevel.KnownSymbol (PreludePath term)
                           )
                        => Parser term
                        -> [FilePath]
                        -> FilePath
                        -> IO (Final effects (Value Precise))
evaluateProjectWithPrelude parser exts entryPoint = do
  let rootDir = takeDirectory entryPoint
  paths <- filter (/= entryPoint) <$> getPaths exts rootDir
  evaluateFilesWithPrelude parser rootDir (entryPoint : paths)

getPaths exts = fmap fold . globDir (compile . mappend "**/*." <$> exts)

-- Evalute a single file.
evaluateFile :: forall term effects
             .  ( Corecursive term
                , Evaluatable (Base term)
                , FreeVariables term
                , effects ~ Effects Precise term (Value Precise) (Evaluating Precise term (Value Precise) effects)
                , MonadAddressable Precise (Evaluating Precise term (Value Precise) effects)
                , Recursive term
                )
             => Parser term
             -> FilePath
             -> IO (Final effects (Value Precise))
evaluateFile parser path = runAnalysis @(Evaluating Precise term (Value Precise)) . evaluateModule <$> parseFile parser Nothing path

evaluateWith :: forall location value term effects
             .  ( Corecursive term
                , effects ~ Effects location term value (Evaluating location term value effects)
                , Evaluatable (Base term)
                , FreeVariables term
                , MonadAddressable location (Evaluating location term value effects)
                , MonadValue location value (Evaluating location term value effects)
                , Recursive term
                , Reducer value (Cell location value)
                , Show location
                )
         => Module term
         -> Module term
         -> Final effects value
evaluateWith prelude m = runAnalysis @(Evaluating location term value) $ do
  -- TODO: we could add evaluatePrelude to MonadAnalysis as an alias for evaluateModule,
  -- overridden in Evaluating to not reset the environment. In the future we'll want the
  -- result of evaluating the Prelude to be a build artifact, rather than something that's
  -- evaluated every single time, but that's contingent upon a whole lot of other future
  -- scaffolding.
  preludeEnv <- evaluateModule prelude *> getEnv
  withDefaultEnvironment preludeEnv (evaluateModule m)

evaluateWithPrelude :: forall term effects
                    .  ( Corecursive term
                       , Evaluatable (Base term)
                       , FreeVariables term
                       , effects ~ Effects Precise term (Value Precise) (Evaluating Precise term (Value Precise) effects)
                       , MonadAddressable Precise (Evaluating Precise term (Value Precise) effects)
                       , Recursive term
                       , TypeLevel.KnownSymbol (PreludePath term)
                       )
                    => Parser term
                    -> FilePath
                    -> IO (Final effects (Value Precise))
evaluateWithPrelude parser path = do
  let preludePath = TypeLevel.symbolVal (Proxy :: Proxy (PreludePath term))
  prelude <- parseFile parser Nothing preludePath
  m <- parseFile parser Nothing path
  pure $ evaluateWith @Precise prelude m


-- Evaluate a list of files (head of file list is considered the entry point).
evaluateFiles :: forall term effects
              .  ( Corecursive term
                 , Evaluatable (Base term)
                 , FreeVariables term
                 , effects ~ Effects Precise term (Value Precise) (Evaluating Precise term (Value Precise) effects)
                 , MonadAddressable Precise (Evaluating Precise term (Value Precise) effects)
                 , Recursive term
                 )
              => Parser term
              -> FilePath
              -> [FilePath]
              -> IO (Final effects (Value Precise))
evaluateFiles parser rootDir paths = runAnalysis @(Evaluating Precise term (Value Precise)) . evaluateModules <$> parseFiles parser rootDir paths

-- | Evaluate terms and an entry point to a value with a given prelude.
evaluatesWith :: forall location value term effects
              .  ( Corecursive term
                 , effects ~ Effects location term value (Evaluating location term value effects)
                 , Evaluatable (Base term)
                 , FreeVariables term
                 , MonadAddressable location (Evaluating location term value effects)
                 , MonadValue location value (Evaluating location term value effects)
                 , Recursive term
                 , Reducer value (Cell location value)
                 , Show location
                 )
              => Module term   -- ^ Prelude to evaluate once
              -> [Module term] -- ^ List of modules that make up the program to be evaluated
              -> Final effects value
evaluatesWith prelude modules = runAnalysis @(Evaluating location term value) $ do
  preludeEnv <- evaluateModule prelude *> getEnv
  withDefaultEnvironment preludeEnv (evaluateModules modules)

evaluateFilesWithPrelude :: forall term effects
                         .  ( Corecursive term
                            , Evaluatable (Base term)
                            , FreeVariables term
                            , effects ~ Effects Precise term (Value Precise) (Evaluating Precise term (Value Precise) effects)
                            , MonadAddressable Precise (Evaluating Precise term (Value Precise) effects)
                            , Recursive term
                            , TypeLevel.KnownSymbol (PreludePath term)
                            )
                         => Parser term
                         -> FilePath
                         -> [FilePath]
                         -> IO (Final effects (Value Precise))
evaluateFilesWithPrelude parser rootDir paths = do
  let preludePath = TypeLevel.symbolVal (Proxy :: Proxy (PreludePath term))
  prelude <- parseFile parser Nothing preludePath
  xs <- parseFiles parser rootDir paths
  pure $ evaluatesWith @Precise @(Value Precise) prelude xs

-- Read and parse a file.
parseFile :: Parser term -> Maybe FilePath -> FilePath -> IO (Module term)
parseFile parser rootDir path = runTask $ do
  blob <- file path
  moduleForBlob rootDir blob <$> parse parser blob

parseFiles :: Parser term -> FilePath -> [FilePath] -> IO [Module term]
parseFiles parser rootDir = traverse (parseFile parser (Just rootDir))

parsePackage :: PackageName -> Parser term -> FilePath -> [FilePath] -> IO (Package term)
parsePackage name parser rootDir = runTask . Task.parsePackage name parser rootDir


-- Read a file from the filesystem into a Blob.
file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

-- Diff helpers
diffWithParser :: ( HasField fields Data.Span.Span
                  , HasField fields Range
                  , Eq1 syntax
                  , Show1 syntax
                  , Traversable syntax
                  , Diffable syntax
                  , GAlign syntax
                  , HasDeclaration syntax
                  )
               => Parser (Term syntax (Record fields))
               -> BlobPair
               -> Task (Diff syntax (Record (Maybe Declaration ': fields)) (Record (Maybe Declaration ': fields)))
diffWithParser parser blobs = distributeFor blobs (\ blob -> WrapTask $ parse parser blob >>= decorate (declarationAlgebra blob)) >>= diffTermPair diffTerms . runJoin
