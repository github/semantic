{-# LANGUAGE CPP, ConstraintKinds, Rank2Types, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util
  ( evalGoProject
  , evalPHPProject
  , evalPythonProject
  , evalRubyProject
  , evalTypeScriptProject
  , evaluateProject'
  , mergeErrors
  , reassociate
  , parseFile
  ) where

import Prelude hiding (readFile)

import           Control.Abstract
import           Control.Abstract.Heap (runHeapError)
import           Control.Abstract.ScopeGraph (runScopeError)
import           Control.Effect.Trace (runTraceByPrinting)
import           Control.Exception (displayException)
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Abstract.Value.Concrete as Concrete
import           Data.Blob
import           Data.File
import           Data.Graph (topologicalSort)
import qualified Data.Language as Language
import           Data.List (uncons)
import           Data.Project hiding (readFile)
import           Data.Quieterm (Quieterm, quieterm)
import           Data.Sum (weaken)
import qualified Language.Go.Assignment
import qualified Language.PHP.Assignment
import qualified Language.Python.Assignment
import qualified Language.Ruby.Assignment
import qualified Language.TypeScript.Assignment
import           Parsing.Parser
import           Prologue
import           Semantic.Analysis
import           Semantic.Config
import           Semantic.Graph
import           Semantic.Task
import           System.Exit (die)
import           System.FilePath.Posix (takeDirectory)

import Data.Location

-- The type signatures in these functions are pretty gnarly, but these functions
-- are hit sufficiently often in the CLI and test suite so as to merit avoiding
-- the overhead of repeated type inference. If you have to hack on these functions,
-- it's recommended to remove all the type signatures and add them back when you
-- are done (type holes in GHCi will help here).

justEvaluating :: Evaluator
                        term
                        Precise
                        (Value term Precise)
                        (ResumableC
                           (BaseError (ValueError term Precise))
                              (ResumableC
                                 (BaseError (AddressError Precise (Value term Precise)))
                                    (ResumableC
                                       (BaseError ResolutionError)
                                          (ResumableC
                                             (BaseError
                                                (EvalError term Precise (Value term Precise)))
                                                (ResumableC
                                                   (BaseError (HeapError Precise))
                                                      (ResumableC
                                                         (BaseError (ScopeError Precise))
                                                            (ResumableC
                                                               (BaseError
                                                                  (UnspecializedError
                                                                     Precise (Value term Precise)))
                                                                  (ResumableC
                                                                     (BaseError
                                                                        (LoadError
                                                                           Precise
                                                                           (Value term Precise)))
                                                                        (FreshC
                                                                              (StateC
                                                                                 (ScopeGraph
                                                                                    Precise)
                                                                                    (StateC
                                                                                       (Heap
                                                                                          Precise
                                                                                          Precise
                                                                                          (Value
                                                                                             term
                                                                                             Precise))
                                                                                          (TraceByPrintingC
                                                                                                (LiftC
                                                                                                   IO)))))))))))))
                        result
                      -> IO
                           (Heap Precise Precise (Value term Precise),
                            (ScopeGraph Precise,
                             Either
                               (SomeError
                                  (Sum
                                     '[BaseError (ValueError term Precise),
                                       BaseError (AddressError Precise (Value term Precise)),
                                       BaseError ResolutionError,
                                       BaseError (EvalError term Precise (Value term Precise)),
                                       BaseError (HeapError Precise),
                                       BaseError (ScopeError Precise),
                                       BaseError (UnspecializedError Precise (Value term Precise)),
                                       BaseError (LoadError Precise (Value term Precise))]))
                               result))
justEvaluating
  = runM
  . runEvaluator
  . raiseHandler runTraceByPrinting
  . runHeap
  . runScopeGraph
  . raiseHandler runFresh
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runScopeError
  . runHeapError
  . runEvalError
  . runResolutionError
  . runAddressError
  . runValueError

type FileEvaluator syntax =
  [FilePath]
  -> IO
       (Heap
          Precise
          Precise
          (Value
             (Quieterm (Sum syntax) Location) Precise),
        (ScopeGraph Precise,
         Either
           (SomeError
              (Sum
                 '[BaseError
                     (ValueError
                        (Quieterm (Sum syntax) Location)
                        Precise),
                   BaseError
                     (AddressError
                        Precise
                        (Value
                           (Quieterm
                              (Sum syntax) Location)
                           Precise)),
                   BaseError ResolutionError,
                   BaseError
                     (EvalError
                        (Quieterm (Sum syntax) Location)
                        Precise
                        (Value
                           (Quieterm
                              (Sum syntax) Location)
                           Precise)),
                   BaseError (HeapError Precise),
                   BaseError (ScopeError Precise),
                   BaseError
                     (UnspecializedError
                        Precise
                        (Value
                           (Quieterm
                              (Sum syntax) Location)
                           Precise)),
                   BaseError
                     (LoadError
                        Precise
                        (Value
                           (Quieterm
                              (Sum syntax) Location)
                           Precise))]))
           (ModuleTable
              (Module
                 (ModuleResult
                    Precise
                    (Value
                       (Quieterm (Sum syntax) Location)
                       Precise))))))

evalGoProject :: FileEvaluator Language.Go.Assignment.Syntax
evalGoProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Go) goParser

evalRubyProject :: FileEvaluator Language.Ruby.Assignment.Syntax
evalRubyProject = justEvaluating <=< evaluateProject (Proxy @'Language.Ruby)               rubyParser

evalPHPProject :: FileEvaluator Language.PHP.Assignment.Syntax
evalPHPProject  = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.PHP)        phpParser

evalPythonProject :: FileEvaluator Language.Python.Assignment.Syntax
evalPythonProject     = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser

evalTypeScriptProject :: FileEvaluator Language.TypeScript.Assignment.Syntax
evalTypeScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser

evaluateProject proxy parser paths = withOptions debugOptions $ \ config logger statter ->
  evaluateProject' (TaskSession config "-" False logger statter) proxy parser paths

-- Evaluate a project consisting of the listed paths.
evaluateProject' session proxy parser paths = do
  res <- runTask session $ do
    blobs <- catMaybes <$> traverse readBlobFromFile (flip File (Language.reflect proxy) <$> paths)
    package <- fmap (quieterm . snd) <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
    modules <- topologicalSort <$> runImportGraphToModules proxy package
    trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
    pure (id @(Evaluator _ Precise (Value _ Precise) _ _)
         (runModuleTable
         (runModules (ModuleTable.modulePaths (packageModules package))
         (raiseHandler (runReader (packageInfo package))
         (raiseHandler (evalState (lowerBound @Span))
         (raiseHandler (runReader (lowerBound @Span))
         (evaluate proxy (runDomainEffects (evalTerm withTermSpans)) modules)))))))
  either (die . displayException) pure res

parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask' . (parse parser <=< readBlob . file)

runTask' :: TaskEff a -> IO a
runTask' task = runTaskWithOptions debugOptions task >>= either (die . displayException) pure

mergeErrors :: Either (SomeError (Sum errs)) (Either (SomeError err) result) -> Either (SomeError (Sum (err ': errs))) result
mergeErrors = either (\ (SomeError sum) -> Left (SomeError (weaken sum))) (either (\ (SomeError err) -> Left (SomeError (inject err))) Right)

reassociate :: Either (SomeError err1) (Either (SomeError err2) (Either (SomeError err3) (Either (SomeError err4) (Either (SomeError err5) (Either (SomeError err6) (Either (SomeError err7) (Either (SomeError err8) result))))))) -> Either (SomeError (Sum '[err8, err7, err6, err5, err4, err3, err2, err1])) result
reassociate = mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . Right
