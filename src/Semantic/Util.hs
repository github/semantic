-- MonoLocalBinds is to silence a warning about a simplifiable constraint.
{-# LANGUAGE DataKinds, MonoLocalBinds, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import Analysis.Abstract.BadVariables
import Analysis.Abstract.BadModuleResolutions
import Analysis.Abstract.BadValues
import Analysis.Abstract.Caching
import Analysis.Abstract.Quiet
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
import Data.Span
import Data.Term
import Diffing.Algorithm
import Diffing.Interpreter
import System.FilePath.Glob
import qualified GHC.TypeLits as TypeLevel
import Language.Preluded
import Parsing.Parser
import Prologue
import Semantic
import Semantic.IO as IO
import Semantic.Task
import System.FilePath.Posix

import qualified Language.Go.Assignment as Go
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript

-- Ruby
evalRubyProject path = runEvaluating <$> (withPrelude <$> parsePrelude rubyParser <*> (evaluatePackageBody <$> parseProject rubyParser ["rb"] path))
evalRubyFile path = runEvaluating <$> (withPrelude <$> parsePrelude rubyParser <*> (evaluateModule <$> parseFile rubyParser Nothing path))

evaluateRubyProjectGraph path = runAnalysis @(ImportGraphing (BadModuleResolutions (BadVariables (BadValues (Quietly (Evaluating (Located Precise Ruby.Term) Ruby.Term (Value (Located Precise Ruby.Term)))))))) . evaluatePackageBody <$> parseProject rubyParser ["rb"] path

evaluateRubyImportGraph paths = runAnalysis @(ImportGraphing (Evaluating (Located Precise Ruby.Term) Ruby.Term (Value (Located Precise Ruby.Term)))) . evaluateModules <$> parseFiles rubyParser (dropFileName (head paths)) paths

evaluateRubyBadVariables paths = runAnalysis @(BadVariables (Evaluating Precise Ruby.Term (Value Precise))) . evaluateModules <$> parseFiles rubyParser (dropFileName (head paths)) paths

-- Go
evalGoProject path = runEvaluating . evaluatePackageBody <$> parseProject goParser ["go"] path
evalGoFile path = runEvaluating . evaluateModule <$> parseFile goParser Nothing path

typecheckGoFile path = runAnalysis @(Caching (Evaluating Monovariant Go.Term Type)) . evaluateModule <$> parseFile goParser Nothing path

-- Python
evalPythonProject path = runEvaluating . evaluatePackageBody <$> parseProject pythonParser ["py"] path
evalPythonFile path = runEvaluating <$> (withPrelude <$> parsePrelude pythonParser <*> (evaluateModule <$> parseFile pythonParser Nothing path))

evaluatePythonImportGraph name paths = runAnalysis @(ImportGraphing (Evaluating (Located Precise Python.Term) Python.Term (Value (Located Precise Python.Term)))) . evaluatePackage <$> parsePackage name pythonParser (dropFileName (head paths)) paths

typecheckPythonFile path = runAnalysis @(Caching (Evaluating Monovariant Python.Term Type)) . evaluateModule <$> parseFile pythonParser Nothing path
tracePythonFile path = runAnalysis @(Tracing [] (Evaluating Precise Python.Term (Value Precise))) . evaluateModule <$> parseFile pythonParser Nothing path
evaluateDeadTracePythonFile path = runAnalysis @(DeadCode (Tracing [] (Evaluating Precise Python.Term (Value Precise)))) . evaluateModule <$> parseFile pythonParser Nothing path

-- PHP
evalPHPProject path = runEvaluating . evaluatePackageBody <$> parseProject phpParser ["php"] path
evalPHPFile path = runEvaluating . evaluateModule <$> parseFile phpParser Nothing path

-- TypeScript
evalTypeScriptProject path = runEvaluating . evaluatePackageBody <$> parseProject typescriptParser ["ts", "tsx"] path
evalTypeScriptFile path = runEvaluating . evaluateModule <$> parseFile typescriptParser Nothing path
typecheckTypeScriptFile path = runAnalysis @(Caching (Evaluating Monovariant TypeScript.Term Type)) . evaluateModule <$> parseFile typescriptParser Nothing path

-- TODO: Remove this by exporting EvaluatingEffects
runEvaluating :: forall term effects a.
                 ( Effects Precise term (Value Precise) (Evaluating Precise term (Value Precise) effects) ~ effects
                 , Corecursive term
                 , Recursive term
                 , Evaluatable (Base term)
                 , FreeVariables term)
              => Evaluating Precise term (Value Precise) effects a
              -> Final effects a
runEvaluating = runAnalysis @(Evaluating Precise term (Value Precise))

parsePrelude :: forall term. TypeLevel.KnownSymbol (PreludePath term) => Parser term -> IO (Module term)
parsePrelude parser = do
  let preludePath = TypeLevel.symbolVal (Proxy :: Proxy (PreludePath term))
  parseFile parser Nothing preludePath

parseProject :: Parser term
                -> [Prelude.String]
                -> FilePath
                -> IO (PackageBody term)
parseProject parser exts entryPoint = do
  let rootDir = takeDirectory entryPoint
  paths <- getPaths exts rootDir
  modules <- parseFiles parser rootDir paths
  pure $ fromModulesWithEntryPoint modules (takeFileName entryPoint)

withPrelude prelude a = do
  preludeEnv <- evaluateModule prelude *> getEnv
  withDefaultEnvironment preludeEnv a

getPaths exts = fmap fold . globDir (compile . mappend "**/*." <$> exts)


-- Read and parse a file.
parseFile :: Parser term -> Maybe FilePath -> FilePath -> IO (Module term)
parseFile parser rootDir path = runTask $ do
  blob <- file path
  moduleForBlob rootDir blob <$> parse parser blob

parseFiles :: Parser term -> FilePath -> [FilePath] -> IO [Module term]
parseFiles parser rootDir = traverse (parseFile parser (Just rootDir))

parsePackage :: PackageName -> Parser term -> FilePath -> [FilePath] -> IO (Package term)
parsePackage name parser rootDir files = Package (PackageInfo name Nothing) . Package.fromModules <$> parseFiles parser rootDir files


-- Read a file from the filesystem into a Blob.
file :: MonadIO m => FilePath -> m Blob
file path = fromJust <$> IO.readFile path (languageForFilePath path)

-- Diff helpers
diffWithParser ::
               ( HasField fields Data.Span.Span
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
diffWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse blobs = bidistributeFor (runJoin blobs) parse parse >>= diffTermPair diffTerms

diffBlobWithParser ::
                   ( HasField fields Data.Span.Span
                   , HasField fields Range
                   , Traversable syntax
                   , HasDeclaration syntax
                   )
                  => Parser (Term syntax (Record fields))
                  -> Blob
                  -> Task (Term syntax (Record (Maybe Declaration : fields)))
diffBlobWithParser parser = run (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob))
  where
    run parse = parse
