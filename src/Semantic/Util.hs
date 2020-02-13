{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-exported-signatures -Wno-partial-type-signatures -O0 #-}
module Semantic.Util
  ( evaluateProject'
  , justEvaluating
  , mergeErrors
  , reassociate
  , parseFile
  , parseFileQuiet
  ) where

import Prelude hiding (readFile)

import           Analysis.File
import           Analysis.Project
import           Control.Abstract
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Lift
import           Control.Carrier.Parse.Simple
import           Control.Carrier.Reader
import           Control.Carrier.Resumable.Either (SomeError (..))
import           Control.Carrier.State.Strict
import           Control.Carrier.Trace.Printing
import           Control.Exception hiding (evaluate)
import           Control.Lens.Getter
import           Control.Monad
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Abstract.Value.Concrete as Concrete
import           Data.Blob.IO
import           Data.Graph.Algebraic (topologicalSort)
import qualified Data.Language as Language
import           Data.List (uncons)
import           Data.Maybe
import           Data.Semilattice.Lower
import           Data.Sum
import           Parsing.Parser
import           Semantic.Analysis
import           Semantic.Config
import           Semantic.Graph
import           Semantic.Task
import           Source.Span (HasSpan (..))
import           System.Exit (die)
import           System.FilePath.Posix (takeDirectory)
import qualified System.Path as Path

justEvaluating :: Evaluator term Precise (Value term Precise) _ result
               -> IO ( Heap Precise Precise (Value term Precise),
                     ( ScopeGraph Precise
                     , Either (SomeError (Sum _)) result)
                     )
justEvaluating
  = runM
  . runEvaluator
  . raiseHandler runTrace
  . runHeap
  . runScopeGraph
  . raiseHandler (fmap snd . runFresh 0)
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runScopeError
  . runHeapError
  . runEvalError
  . runResolutionError
  . runAddressError
  . runValueError

-- Evaluate a project consisting of the listed paths.
evaluateProject' session proxy parser paths = do
  let lang = Language.reflect proxy
  res <- runTask session $ asks configTreeSitterParseTimeout >>= \ timeout -> runParse timeout $ do
    blobs <- catMaybes <$> traverse readBlobFromFile (fileForPath <$> paths)
    package <- fmap snd <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs lang [])
    modules <- topologicalSort <$> runImportGraphToModules proxy package
    trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
    pure (package, modules)
  (package, modules) <- either (die . displayException) pure res
  pure (runModuleTable
       (runModules (ModuleTable.modulePaths (packageModules package))
       (raiseHandler (runReader (packageInfo package))
       (raiseHandler (evalState (lowerBound @Span))
       (raiseHandler (runReader (lowerBound @Span))
       (evaluate proxy (runDomainEffects (evalTerm (withTermSpans (^. span_)))) modules))))))

parseFile, parseFileQuiet :: Parser term -> FilePath -> IO term
parseFile      parser = runTask'     . (parse parser <=< readBlob . fileForPath)
parseFileQuiet parser = runTaskQuiet . (parse parser <=< readBlob . fileForPath)

fileForPath :: FilePath -> File Language.Language
fileForPath (Path.absRel -> p) = File p lowerBound (Language.forPath p)

runTask', runTaskQuiet :: ParseC TaskC a -> IO a
runTask'     task = runTaskWithOptions debugOptions   (asks configTreeSitterParseTimeout >>= \ timeout -> runParse timeout task) >>= either (die . displayException) pure
runTaskQuiet task = runTaskWithOptions defaultOptions (asks configTreeSitterParseTimeout >>= \ timeout -> runParse timeout task) >>= either (die . displayException) pure

mergeErrors :: Either (SomeError (Sum errs)) (Either (SomeError err) result) -> Either (SomeError (Sum (err ': errs))) result
mergeErrors = either (\ (SomeError sum) -> Left (SomeError (weaken sum))) (either (\ (SomeError err) -> Left (SomeError (inject err))) Right)

reassociate :: Either (SomeError err1) (Either (SomeError err2) (Either (SomeError err3) (Either (SomeError err4) (Either (SomeError err5) (Either (SomeError err6) (Either (SomeError err7) (Either (SomeError err8) result))))))) -> Either (SomeError (Sum '[err8, err7, err6, err5, err4, err3, err2, err1])) result
reassociate = mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . Right
