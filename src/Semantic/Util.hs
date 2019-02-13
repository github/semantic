{-# LANGUAGE Rank2Types, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists #-}
module Semantic.Util where

import Prelude hiding (readFile)

import           Analysis.Abstract.Caching.FlowSensitive
import           Analysis.Abstract.Collecting
import           Control.Abstract
import           Control.Abstract.Heap (runHeapError)
import           Control.Abstract.ScopeGraph (runScopeError)
import           Control.Effect.Trace (runTraceByPrinting)
import           Control.Exception (displayException)
import           Data.Abstract.Address.Hole as Hole
import           Data.Abstract.Address.Monovariant as Monovariant
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Abstract.Value.Concrete as Concrete
import           Data.Abstract.Value.Type as Type
import           Data.Blob
import           Data.File
import           Data.Graph (topologicalSort)
import           Data.Graph.ControlFlowVertex
import qualified Data.Language as Language
import           Data.List (uncons)
import           Data.Project hiding (readFile)
import           Data.Sum (weaken)
import           Data.Term
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
import Data.Quieterm

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
  :: Evaluator
       term
       Monovariant
       Type.Type
       (ResumableC
          (BaseError
             Type.TypeError)
          (Eff
             (StateC
                Type.TypeMap
                (Eff
                   (ResumableC
                      (BaseError
                         (AddressError
                            Monovariant
                            Type.Type))
                      (Eff
                         (ResumableC
                            (BaseError
                               (EvalError
                                  term
                                  Monovariant
                                  Type.Type))
                            (Eff
                               (ResumableC
                                  (BaseError
                                     ResolutionError)
                                  (Eff
                                     (ResumableC
                                        (BaseError
                                           (HeapError
                                              Monovariant))
                                        (Eff
                                           (ResumableC
                                              (BaseError
                                                 (ScopeError
                                                    Monovariant))
                                              (Eff
                                                 (ResumableC
                                                    (BaseError
                                                       (UnspecializedError
                                                          Monovariant
                                                          Type.Type))
                                                    (Eff
                                                       (ResumableC
                                                          (BaseError
                                                             (LoadError
                                                                Monovariant
                                                                Type.Type))
                                                          (Eff
                                                             (ReaderC
                                                                (Live
                                                                   Monovariant)
                                                                (Eff
                                                                   (AltC
                                                                      []
                                                                      (Eff
                                                                         (ReaderC
                                                                            (Cache
                                                                               term
                                                                               Monovariant
                                                                               Type.Type)
                                                                            (Eff
                                                                               (StateC
                                                                                  (Cache
                                                                                     term
                                                                                     Monovariant
                                                                                     Type.Type)
                                                                                  (Eff
                                                                                     (FreshC
                                                                                        (Eff
                                                                                           (StateC
                                                                                              (ScopeGraph
                                                                                                 Monovariant)
                                                                                              (Eff
                                                                                                 (StateC
                                                                                                    (Heap
                                                                                                       Monovariant
                                                                                                       Monovariant
                                                                                                       Type.Type)
                                                                                                    (Eff
                                                                                                       (TraceByPrintingC
                                                                                                          (Eff
                                                                                                             (LiftC
                                                                                                                IO)))))))))))))))))))))))))))))))))))
       result
     -> IO
          (Heap
             Monovariant
             Monovariant
             Type.Type,
           (ScopeGraph
              Monovariant,
            (Cache
               term
               Monovariant
               Type.Type,
             [Either
                (SomeError
                   (Sum
                      '[BaseError
                          Type.TypeError,
                        BaseError
                          (AddressError
                             Monovariant
                             Type.Type),
                        BaseError
                          (EvalError
                             term
                             Monovariant.Monovariant
                             Type.Type),
                        BaseError
                          ResolutionError,
                        BaseError
                          (HeapError
                             Monovariant),
                        BaseError
                          (ScopeError
                             Monovariant),
                        BaseError
                          (UnspecializedError
                             Monovariant
                             Type.Type),
                        BaseError
                          (LoadError
                             Monovariant
                             Type.Type)]))
                result])))
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

evalJavaScriptProject :: FileEvaluator Language.TypeScript.Assignment.Syntax
evalJavaScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.JavaScript) typescriptParser

evalTypeScriptProject :: FileEvaluator Language.TypeScript.Assignment.Syntax
evalTypeScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser

typecheckGoFile :: ( syntax ~ Language.Go.Assignment.Syntax
                   , qterm ~ Quieterm (Sum syntax) Location
                   , value ~ Type
                   , address ~ Monovariant
                   , result ~ (ModuleTable (Module (ModuleResult address value)))) => FilePath
     -> IO
          (Heap
             address
             address
             value,
           (ScopeGraph
              address,
            (Cache
               qterm
               address
               value,
             [Either
                (SomeError
                   (Sum
                      '[BaseError
                          Type.TypeError,
                        BaseError
                          (AddressError
                             address
                             value),
                        BaseError
                          (EvalError
                             qterm
                             address
                             value),
                        BaseError
                          ResolutionError,
                        BaseError
                          (HeapError
                             address),
                        BaseError
                          (ScopeError
                             address),
                        BaseError
                          (UnspecializedError
                             address
                             value),
                        BaseError
                          (LoadError
                             address
                             value)]))
                result])))
typecheckGoFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Go) goParser
typecheckRubyFile :: ( syntax ~ Language.Ruby.Assignment.Syntax
                   , qterm ~ Quieterm (Sum syntax) Location
                   , value ~ Type
                   , address ~ Monovariant
                   , result ~ (ModuleTable (Module (ModuleResult address value)))) => FilePath
     -> IO
          (Heap
             address
             address
             value,
           (ScopeGraph
              address,
            (Cache
               qterm
               address
               value,
             [Either
                (SomeError
                   (Sum
                      '[BaseError
                          Type.TypeError,
                        BaseError
                          (AddressError
                             address
                             value),
                        BaseError
                          (EvalError
                             qterm
                             address
                             value),
                        BaseError
                          ResolutionError,
                        BaseError
                          (HeapError
                             address),
                        BaseError
                          (ScopeError
                             address),
                        BaseError
                          (UnspecializedError
                             address
                             value),
                        BaseError
                          (LoadError
                             address
                             value)]))
                result])))
typecheckRubyFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Ruby) rubyParser

callGraphProject
  :: (Language.SLanguage lang, Ord1 syntax,
      Declarations1 syntax,
      Evaluatable syntax,
      FreeVariables1 syntax,
      AccessControls1 syntax,
      HasPrelude lang, Functor syntax,
      VertexDeclarationWithStrategy
        (VertexDeclarationStrategy syntax)
        syntax
        syntax) =>
     Parser
       (Term syntax Location)
     -> Proxy lang
     -> [FilePath]
     -> IO
          (Graph ControlFlowVertex,
           [Module ()])
callGraphProject parser proxy paths = runTask' $ do
  blobs <- catMaybes <$> traverse readBlobFromFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap snd <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  x <- runCallGraph proxy False modules package
  pure (x, (() <$) <$> modules)

scopeGraphRubyProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.Ruby) rubyParser
scopeGraphPHPProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.PHP) phpParser
scopeGraphGoProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.Go) goParser
scopeGraphTypeScriptProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.TypeScript) typescriptParser
scopeGraphJavaScriptProject = justEvaluatingCatchingErrors <=< evaluateProjectForScopeGraph (Proxy @'Language.TypeScript) typescriptParser

evaluatePythonProject :: ( syntax ~ Language.Python.Assignment.Syntax
                   , qterm ~ Quieterm (Sum syntax) Location
                   , value ~ (Concrete.Value qterm address)
                   , address ~ Precise
                   , result ~ (ModuleTable (Module (ModuleResult address value)))) => FilePath
     -> IO
          (Heap address address value,
           (ScopeGraph address,
             Either
                (SomeError
                   (Sum
                      '[BaseError
                          (ValueError qterm address),
                        BaseError
                          (AddressError
                             address
                             value),
                        BaseError
                          ResolutionError,
                        BaseError
                          (EvalError
                             qterm
                             address
                             value),
                        BaseError
                          (HeapError
                             address),
                        BaseError
                          (ScopeError
                             address),
                        BaseError
                          (UnspecializedError
                             address
                             value),
                        BaseError
                          (LoadError
                             address
                             value)]))
                result))
evaluatePythonProject = justEvaluating <=< evaluatePythonProjects (Proxy @'Language.Python) pythonParser Language.Python

callGraphRubyProject :: [FilePath] -> IO (Graph ControlFlowVertex, [Module ()])
callGraphRubyProject = callGraphProject rubyParser (Proxy @'Language.Ruby)

type EvalEffects qterm = ResumableC (BaseError (ValueError qterm Precise))
                         (Eff (ResumableC (BaseError (AddressError Precise (Value qterm Precise)))
                         (Eff (ResumableC (BaseError ResolutionError)
                         (Eff (ResumableC (BaseError (EvalError qterm Precise (Value qterm Precise)))
                         (Eff (ResumableC (BaseError (HeapError Precise))
                         (Eff (ResumableC (BaseError (ScopeError Precise))
                         (Eff (ResumableC (BaseError (UnspecializedError Precise (Value qterm Precise)))
                         (Eff (ResumableC (BaseError (LoadError Precise (Value qterm Precise)))
                         (Eff (FreshC (Eff (StateC (ScopeGraph Precise)
                         (Eff (StateC (Heap Precise Precise (Value qterm Precise))
                         (Eff (TraceByPrintingC
                         (Eff (LiftC IO))))))))))))))))))))))))

evaluateProject :: ( term ~ Term (Sum syntax) Location
                    , qterm ~ Quieterm (Sum syntax) Location
                    , Language.SLanguage lang
                    , HasPrelude lang
                    , Apply Eq1 syntax
                    , Apply Ord1 syntax
                    , Apply Show1 syntax
                    , Apply Functor syntax
                    , Apply Foldable syntax
                    , Apply Evaluatable syntax
                    , Apply Declarations1 syntax
                    , Apply AccessControls1 syntax
                    , Apply FreeVariables1 syntax
                    )
                 => Proxy lang
                 -> Parser term
                 -> [FilePath]
                 -> IO (Evaluator qterm Precise
                         (Value qterm Precise)
                         (EvalEffects qterm)
                       (ModuleTable (Module
                          (ModuleResult Precise (Value qterm Precise)))))
evaluateProject proxy parser paths = withOptions debugOptions $ \ config logger statter ->
  evaluateProject' (TaskSession config "-" logger statter) proxy parser paths

-- Evaluate a project consisting of the listed paths.
-- TODO: This is used by our specs and should be moved into SpecHelpers.hs
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

evaluatePythonProjects :: ( term ~ Term (Sum Language.Python.Assignment.Syntax) Location
                          , qterm ~ Quieterm (Sum Language.Python.Assignment.Syntax) Location
                          )
                       => Proxy 'Language.Python
                       -> Parser term
                       -> Language.Language
                       -> FilePath
                       -> IO (Evaluator qterm Precise
                               (Value qterm Precise)
                               (ResumableC (BaseError (ValueError qterm Precise))
                               (Eff (ResumableC (BaseError (AddressError Precise (Value qterm Precise)))
                               (Eff (ResumableC (BaseError ResolutionError)
                               (Eff (ResumableC (BaseError (EvalError qterm Precise (Value qterm Precise)))
                               (Eff (ResumableC (BaseError (HeapError Precise))
                               (Eff (ResumableC (BaseError (ScopeError Precise))
                               (Eff (ResumableC (BaseError (UnspecializedError Precise (Value qterm Precise)))
                               (Eff (ResumableC (BaseError (LoadError Precise (Value qterm Precise)))
                               (Eff (FreshC (Eff (StateC (ScopeGraph Precise)
                               (Eff (StateC (Heap Precise Precise (Value qterm Precise))
                               (Eff (TraceByPrintingC
                               (Eff (LiftC IO)))))))))))))))))))))))))
                             (ModuleTable (Module
                                (ModuleResult Precise (Value qterm Precise)))))
evaluatePythonProjects proxy parser lang path = runTask' $ do
  project <- readProject Nothing path lang []
  package <- fmap quieterm <$> parsePythonPackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
  pure (id @(Evaluator _ Precise (Value _ Precise) _ _)
       (runModuleTable
       (runModules (ModuleTable.modulePaths (packageModules package))
       (raiseHandler (runReader (packageInfo package))
       (raiseHandler (evalState (lowerBound @Span))
       (raiseHandler (runReader (lowerBound @Span))
       (evaluate proxy (runDomainEffects (evalTerm withTermSpans)) modules)))))))

evaluateProjectForScopeGraph proxy parser project = runTask' $ do
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


evaluateProjectWithCaching :: ( Language.SLanguage lang
                              , HasPrelude lang
                              , Apply Eq1 syntax
                              , Apply Ord1 syntax
                              , Apply Show1 syntax
                              , Apply Functor syntax
                              , Apply Foldable syntax
                              , Apply Evaluatable syntax
                              , Apply Declarations1 syntax
                              , Apply AccessControls1 syntax
                              , Apply FreeVariables1 syntax
                              , term ~ Term (Sum syntax) Location
                              , qterm ~ Quieterm (Sum syntax) Location
                              )
                           => Proxy (lang :: Language.Language)
                           -> Parser term
                          -> FilePath
                          -> IO (Evaluator qterm Monovariant Type
                                  (ResumableC (BaseError Type.TypeError)
                                  (Eff (StateC TypeMap
                                  (Eff (ResumableC (BaseError (AddressError Monovariant Type))
                                  (Eff (ResumableC (BaseError (EvalError qterm Monovariant Type))
                                  (Eff (ResumableC (BaseError ResolutionError)
                                  (Eff (ResumableC (BaseError (HeapError Monovariant))
                                  (Eff (ResumableC (BaseError (ScopeError Monovariant))
                                  (Eff (ResumableC (BaseError (UnspecializedError Monovariant Type))
                                  (Eff (ResumableC (BaseError (LoadError Monovariant Type))
                                  (Eff (ReaderC (Live Monovariant)
                                  (Eff (AltC []
                                  (Eff (ReaderC (Analysis.Abstract.Caching.FlowSensitive.Cache (Data.Quieterm.Quieterm (Sum syntax) Data.Location.Location) Monovariant Type)
                                  (Eff (StateC (Analysis.Abstract.Caching.FlowSensitive.Cache (Data.Quieterm.Quieterm (Sum syntax) Data.Location.Location) Monovariant Type)
                                  (Eff (FreshC
                                  (Eff (StateC (ScopeGraph Monovariant)
                                  (Eff (StateC (Heap Monovariant Monovariant Type)
                                  (Eff (TraceByPrintingC (Eff (LiftC IO)))))))))))))))))))))))))))))))))))
                                 (ModuleTable (Module (ModuleResult Monovariant Type))))
evaluateProjectWithCaching proxy parser path = runTask' $ do
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
parseFile parser = runTask' . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask' . readBlob . file

runTask' :: TaskEff a -> IO a
runTask' task = runTaskWithOptions debugOptions task >>= either (die . displayException) pure

mergeErrors :: Either (SomeError (Sum errs)) (Either (SomeError err) result) -> Either (SomeError (Sum (err ': errs))) result
mergeErrors = either (\ (SomeError sum) -> Left (SomeError (weaken sum))) (either (\ (SomeError err) -> Left (SomeError (inject err))) Right)

reassociate :: Either (SomeError err1) (Either (SomeError err2) (Either (SomeError err3) (Either (SomeError err4) (Either (SomeError err5) (Either (SomeError err6) (Either (SomeError err7) (Either (SomeError err8) result))))))) -> Either (SomeError (Sum '[err8, err7, err6, err5, err4, err3, err2, err1])) result
reassociate = mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . Right
