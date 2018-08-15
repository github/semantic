{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists #-}
module Semantic.Util where

import Prelude hiding (readFile)

import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Control.Abstract
import           Control.Exception (displayException)
import           Control.Monad.Effect.Trace (runPrintingTrace)
import           Data.Abstract.Address.Monovariant as Monovariant
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.BaseError (BaseError(..))
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Abstract.Value.Concrete as Concrete
import           Data.Abstract.Value.Type as Type
import           Data.Blob
import           Data.Coerce
import           Data.Graph (topologicalSort)
import qualified Data.Language as Language
import           Data.List (uncons)
import           Data.Project hiding (readFile)
import           Data.Quieterm (quieterm)
import           Data.Sum (weaken)
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Parsing.Parser
import           Prologue hiding (weaken)
import           Semantic.Config
import           Semantic.Graph
import           Semantic.IO as IO
import           Semantic.Task
import           Semantic.Telemetry (LogQueue, StatQueue)
import           System.Exit (die)
import           System.FilePath.Posix (takeDirectory)
import           Text.Show.Pretty (ppShow)

justEvaluating
  = runM
  . runPrintingTrace
  . runState lowerBound
  . runFresh 0
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runEnvironmentError
  . runEvalError
  . runResolutionError
  . runAddressError
  . runValueError

newtype UtilEff address rest a = UtilEff
  { runUtilEff :: Eff (  ValueEffects  address (Value address (UtilEff address rest))
                      (  ModuleEffects address (Value address (UtilEff address rest))
                      (  Reader Span
                      ': Reader PackageInfo
                      ': Modules address
                      ': Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))
                      ': Resumable (BaseError (ValueError address (UtilEff address rest)))
                      ': Resumable (BaseError (AddressError address (Value address (UtilEff address rest))))
                      ': Resumable (BaseError ResolutionError)
                      ': Resumable (BaseError EvalError)
                      ': Resumable (BaseError (EnvironmentError address))
                      ': Resumable (BaseError (UnspecializedError (Value address (UtilEff address rest))))
                      ': Resumable (BaseError (LoadError address))
                      ': Fresh
                      ': State (Heap address (Value address (UtilEff address rest)))
                      ': rest
                      ))) a
  }

checking
  = runM @_ @IO
  . runPrintingTrace
  . runState (lowerBound @(Heap Monovariant Type))
  . runFresh 0
  . runTermEvaluator @_ @Monovariant @Type
  . caching
  . providingLiveSet
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runResolutionError
  . runEnvironmentError
  . runEvalError
  . runAddressError
  . runTypes

evalGoProject         = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Go)         goParser
evalRubyProject       = justEvaluating <=< evaluateProject (Proxy @'Language.Ruby)       rubyParser
evalPHPProject        = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.PHP)        phpParser
evalPythonProject     = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser
evalJavaScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.JavaScript) typescriptParser
evalTypeScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser

typecheckGoFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Go) goParser
typecheckRubyFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Ruby) rubyParser

callGraphProject parser proxy opts paths = runTaskWithOptions opts $ do
  blobs <- catMaybes <$> traverse readFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap snd <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  x <- runCallGraph proxy False modules package
  pure (x, (() <$) <$> modules)

callGraphRubyProject = callGraphProject rubyParser (Proxy @'Language.Ruby) debugOptions

-- Evaluate a project consisting of the listed paths.
evaluateProject proxy parser paths = withOptions debugOptions $ \ config logger statter ->
  evaluateProject' (TaskConfig config logger statter) proxy parser paths

data TaskConfig = TaskConfig Config LogQueue StatQueue

evaluateProject' (TaskConfig config logger statter) proxy parser paths = either (die . displayException) pure <=< runTaskWithConfig config logger statter $ do
  blobs <- catMaybes <$> traverse readFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap (quieterm . snd) <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
  pure (runTermEvaluator @_ @_ @(Value Precise (UtilEff Precise _))
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (runReader (packageInfo package)
       (runReader (lowerBound @Span)
       (evaluate proxy id withTermSpans (Precise.runAllocator . Precise.runDeref) (Concrete.runFunction coerce coerce) modules))))))


evaluateProjectWithCaching proxy parser path = runTaskWithOptions debugOptions $ do
  project <- readProject Nothing path (Language.reflect proxy) []
  package <- fmap (quieterm . snd) <$> parsePackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  pure (runReader (packageInfo package)
       (runReader (lowerBound @Span)
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Monovariant)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (evaluate proxy id withTermSpans (Monovariant.runAllocator . Monovariant.runDeref) Type.runFunction modules)))))


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file


mergeExcs :: Either (SomeExc (Sum excs)) (Either (SomeExc exc) result) -> Either (SomeExc (Sum (exc ': excs))) result
mergeExcs = either (\ (SomeExc sum) -> Left (SomeExc (weaken sum))) (either (\ (SomeExc exc) -> Left (SomeExc (inject exc))) Right)

reassociate :: Either (SomeExc exc1) (Either (SomeExc exc2) (Either (SomeExc exc3) (Either (SomeExc exc4) (Either (SomeExc exc5) (Either (SomeExc exc6) (Either (SomeExc exc7) result)))))) -> Either (SomeExc (Sum '[exc7, exc6, exc5, exc4, exc3, exc2, exc1])) result
reassociate = mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . Right


prettyShow :: Show a => a -> IO ()
prettyShow = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow
