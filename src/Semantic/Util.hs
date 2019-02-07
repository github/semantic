{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists #-}
module Semantic.Util where

import Prelude hiding (readFile)

import           Analysis.Abstract.Caching.FlowSensitive
import           Analysis.Abstract.Collecting
import           Control.Abstract
import           Control.Abstract.Heap (runHeapError)
import           Control.Abstract.ScopeGraph (runScopeError)
import           Control.Exception (displayException)
import           Control.Effect.Trace (runTraceByPrinting)
import           Data.Abstract.Address.Monovariant as Monovariant
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.Address.Hole as Hole
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Abstract.Value.Concrete as Concrete
import           Data.Abstract.Value.Type as Type
import           Data.Blob
import           Data.File
import           Data.Graph (topologicalSort)
import qualified Data.Language as Language
import           Data.List (uncons)
import           Data.Project hiding (readFile)
import           Data.Sum (weaken)
import           Parsing.Parser
import           Prologue
import           Semantic.Analysis
import           Semantic.Config
import           Semantic.Graph
import           Semantic.Task
import           Semantic.Telemetry (LogQueue, StatQueue)
import           System.Exit (die)
import           System.FilePath.Posix (takeDirectory)

import Data.Quieterm
import Data.Location

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

-- We can't go with the inferred type because this needs to be
-- polymorphic in @lang@.
justEvaluatingCatchingErrors :: ( hole ~ Hole (Maybe Name) Precise
                                , term ~ Quieterm (Sum lang) Location
                                , value ~ Concrete.Value term hole
                                , Apply Show1 lang
                                )
  => Evaluator term hole
       value
       (ResumableWithC
          (BaseError (ValueError term hole))
          (Eff (ResumableWithC (BaseError (AddressError hole value))
          (Eff (ResumableWithC (BaseError ResolutionError)
          (Eff (ResumableWithC (BaseError (EvalError term hole value))
          (Eff (ResumableWithC (BaseError (HeapError hole))
          (Eff (ResumableWithC (BaseError (ScopeError hole))
          (Eff (ResumableWithC (BaseError (UnspecializedError hole value))
          (Eff (ResumableWithC (BaseError (LoadError hole value))
          (Eff (FreshC
          (Eff (StateC (ScopeGraph hole)
          (Eff (StateC (Heap hole hole (Concrete.Value (Quieterm (Sum lang) Location) (Hole (Maybe Name) Precise)))
          (Eff (TraceByPrintingC
          (Eff (LiftC IO))))))))))))))))))))))))) a
     -> IO (Heap hole hole value, (ScopeGraph hole, a))
justEvaluatingCatchingErrors
  = runM
  . runEvaluator @_ @_ @(Value _ (Hole.Hole (Maybe Name) Precise))
  . raiseHandler runTraceByPrinting
  . runHeap
  . runScopeGraph
  . raiseHandler runFresh
  . resumingLoadError
  . resumingUnspecialized
  . resumingScopeError
  . resumingHeapError
  . resumingEvalError
  . resumingResolutionError
  . resumingAddressError
  . resumingValueError

checking
  = runM
  . runEvaluator
  . raiseHandler runTraceByPrinting
  . runHeap
  . runScopeGraph
  . raiseHandler runFresh
  . caching
  . providingLiveSet
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runScopeError
  . runHeapError
  . runResolutionError
  . runEvalError
  . runAddressError
  . runTypes

evalGoProject         = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Go)         goParser
evalRubyProject       = justEvaluating <=< evaluateProject (Proxy @'Language.Ruby)               rubyParser
evalPHPProject        = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.PHP)        phpParser
evalPythonProject     = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser
evalJavaScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.JavaScript) typescriptParser
evalTypeScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser

typecheckGoFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Go) goParser
typecheckRubyFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Ruby) rubyParser

callGraphProject parser proxy opts paths = runTaskWithOptions opts $ do
  blobs <- catMaybes <$> traverse readBlobFromFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap snd <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  x <- runCallGraph proxy False modules package
  pure (x, (() <$) <$> modules)

evaluatePythonProject = justEvaluatingCatchingErrors <=< evaluatePythonProjects (Proxy @'Language.Python) pythonParser Language.Python

scopeGraphPythonProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.Python) pythonParser
scopeGraphRubyProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.Ruby) rubyParser
scopeGraphPHPProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.PHP) phpParser
scopeGraphGoProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.Go) goParser
scopeGraphTypeScriptProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.TypeScript) typescriptParser
scopeGraphJavaScriptProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.TypeScript) typescriptParser

callGraphRubyProject = callGraphProject rubyParser (Proxy @'Language.Ruby) debugOptions

-- Evaluate a project consisting of the listed paths.
evaluateProject proxy parser paths = withOptions debugOptions $ \ config logger statter ->
  evaluateProject' (TaskConfig config logger statter) proxy parser paths

data TaskConfig = TaskConfig Config LogQueue StatQueue

evaluateProject' (TaskConfig config logger statter) proxy parser paths = either (die . displayException) pure <=< runTaskWithConfig config logger statter $ do
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

evaluatePythonProjects proxy parser lang path = runTaskWithOptions debugOptions $ do
  project <- readProject Nothing path lang []
  package <- fmap quieterm <$> parsePythonPackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
  pure (id @(Evaluator _ (Hole.Hole (Maybe Name) Precise) (Value _ (Hole.Hole (Maybe Name) Precise)) _ _)
       (runModuleTable
       (runModules (ModuleTable.modulePaths (packageModules package))
       (raiseHandler (runReader (packageInfo package))
       (raiseHandler (evalState (lowerBound @Span))
       (raiseHandler (runReader (lowerBound @Span))
       (evaluate proxy (runDomainEffects (evalTerm withTermSpans)) modules)))))))

evaluateProjectForScopeGraph proxy parser project = runTaskWithOptions debugOptions $ do
  package <- fmap quieterm <$> parsePythonPackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
  pure (id @(Evaluator _ (Hole.Hole (Maybe Name) Precise) (Value _ (Hole.Hole (Maybe Name) Precise)) _ _)
       (runModuleTable
       (runModules (ModuleTable.modulePaths (packageModules package))
       (raiseHandler (runReader (packageInfo package))
       (raiseHandler (evalState (lowerBound @Span))
       (raiseHandler (runReader (lowerBound @Span))
       (evaluate proxy (runDomainEffects (evalTerm withTermSpans)) modules)))))))

evaluateProjectWithCaching proxy parser path = runTaskWithOptions debugOptions $ do
  project <- readProject Nothing path (Language.reflect proxy) []
  package <- fmap (quieterm . snd) <$> parsePackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  pure (id @(Evaluator _ Monovariant _ _ _)
       (raiseHandler (runReader (packageInfo package))
       (raiseHandler (evalState (lowerBound @Span))
       (raiseHandler (runReader (lowerBound @Span))
       (runModuleTable
       (runModules (ModuleTable.modulePaths (packageModules package))
       (evaluate proxy (runDomainEffects (evalTerm withTermSpans)) modules)))))))


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file

mergeErrors :: Either (SomeError (Sum errs)) (Either (SomeError err) result) -> Either (SomeError (Sum (err ': errs))) result
mergeErrors = either (\ (SomeError sum) -> Left (SomeError (weaken sum))) (either (\ (SomeError err) -> Left (SomeError (inject err))) Right)

reassociate :: Either (SomeError err1) (Either (SomeError err2) (Either (SomeError err3) (Either (SomeError err4) (Either (SomeError err5) (Either (SomeError err6) (Either (SomeError err7) (Either (SomeError err8) result))))))) -> Either (SomeError (Sum '[err8, err7, err6, err5, err4, err3, err2, err1])) result
reassociate = mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . Right
