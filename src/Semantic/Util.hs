{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists #-}
module Semantic.Util where

import Prelude hiding (id, readFile, (.))

import           Analysis.Abstract.Caching.FlowSensitive
import           Analysis.Abstract.Collecting
import           Control.Abstract hiding (null)
import           Control.Category
import           Control.Exception (displayException)
import           Control.Monad.Effect.Trace (runPrintingTrace)
import           Data.Abstract.Address.Monovariant as Monovariant
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.Evaluatable hiding (null)
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
import           Text.Show.Pretty (ppShow, pPrint)

import           Control.Rewriting as Rewriting
import qualified Data.ByteString as B
import           Data.History
import           Data.Record
import qualified Data.Source as Source
import qualified Data.Syntax.Literal as Literal
import           Data.Term
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Language.JSON.PrettyPrint
import           Reprinting.Pipeline

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

evaluatePythonProject = evaluatePythonProjects (Proxy @'Language.Python) pythonParser Language.Python

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
  pure (runTermEvaluator @_ @_ @(Value Precise (ConcreteEff Precise _))
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (runReader (packageInfo package)
       (runState (lowerBound @Span)
       (runReader (lowerBound @Span)
       (evaluate proxy id withTermSpans (Precise.runAllocator . Precise.runDeref) (Concrete.runBoolean . Concrete.runFunction coerce coerce) modules)))))))

evaluatePythonProjects proxy parser lang path = runTaskWithOptions debugOptions $ do
  project <- readProject Nothing path lang []
  package <- fmap quieterm <$> parsePythonPackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
  pure (runTermEvaluator @_ @_ @(Value Precise (ConcreteEff Precise '[Trace]))
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (runReader (packageInfo package)
       (runState (lowerBound @Span)
       (runReader (lowerBound @Span)
       (evaluate proxy id withTermSpans (Precise.runAllocator . Precise.runDeref) (Concrete.runBoolean . Concrete.runFunction coerce coerce) modules)))))))


evaluateProjectWithCaching proxy parser path = runTaskWithOptions debugOptions $ do
  project <- readProject Nothing path (Language.reflect proxy) []
  package <- fmap (quieterm . snd) <$> parsePackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  pure (runReader (packageInfo package)
       (runState (lowerBound @Span)
       (runReader (lowerBound @Span)
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Monovariant)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (evaluate proxy id withTermSpans (Monovariant.runAllocator . Monovariant.runDeref) (Type.runBoolean . Type.runFunction) modules))))))


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

onTrees' :: ( Literal.TextElement :< syn
            , Literal.KeyValue :< syn
            , Apply Functor syn
            , term ~ Term (Sum syn) (Record (History : fields))
            ) => Rewrite (env, term) m (Literal.Hash term)
onTrees' = do
  Literal.Hash els <- Rewriting.target
  guard (null els)
  k <- modified $ Literal.TextElement "\"hi\""
  v <- modified $ Literal.TextElement "\"bye\""
  pair <- modified $ Literal.KeyValue k v
  pure (Literal.Hash (pair : els))

testJson = do
  let path = "test/fixtures/json/rewriting/add_keys.json"

  bytes <- Source.fromUTF8 <$> B.readFile path
  json <- parseFile jsonParser path

  refactored <-
    case applyPure (somewhere' onTrees' markRefactored) () (mark Unmodified json) of
      Left  l -> Prelude.fail (show l)
      -- there are three dictionaries in add_keys.json, and they should all
      -- have a 'hi' key.
      Right r -> pure r

  let res = runReprinter bytes defaultJSONPipeline refactored
  T.putStrLn "*****tokenizing*****"
  pPrint $ runTokenizing bytes refactored
  T.putStrLn "*****contextualizing*****"
  pPrint $ runContextualizing bytes refactored
  T.putStrLn "*****translating*****"
  pPrint $ runTranslating bytes defaultJSONPipeline refactored
  T.putStrLn "*****source*****"
  T.putStrLn $ either (T.pack . show) Source.toText res

  T.putStrLn "*****returning tree*****"
  pure refactored
