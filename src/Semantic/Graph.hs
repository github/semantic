{-# LANGUAGE GADTs #-}
module Semantic.Graph where

import           Analysis.Abstract.BadAddresses
import           Analysis.Abstract.BadModuleResolutions
import           Analysis.Abstract.BadSyntax
import           Analysis.Abstract.BadValues
import           Analysis.Abstract.BadVariables
import           Analysis.Abstract.Erroring
import           Analysis.Abstract.Evaluating
import           Analysis.Abstract.ImportGraph
import           Analysis.Abstract.PythonPackage
import           Analysis.Abstract.CallGraph
import           Analysis.Abstract.Graph (Graph, renderGraph)
import qualified Control.Exception as Exc
import           Data.Abstract.Address
import qualified Data.Abstract.Evaluatable as Analysis
import           Data.Abstract.FreeVariables
import           Data.Abstract.Located
import           Data.Abstract.Module
import           Data.Abstract.Package as Package
import           Data.Abstract.Value (Value)
import           Data.File
import           Data.Output
import qualified Data.Syntax as Syntax
import           Data.Term
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Rendering.Renderer
import           Semantic.IO (Files)
import           Semantic.Task
import System.FilePath.Posix

data GraphType = ImportGraph | CallGraph

graph :: Members '[Distribute WrappedTask, Files, Task, Exc SomeException, Telemetry] effs
      => GraphType
      -> GraphRenderer output
      -> Project
      -> Eff effs ByteString
graph graphType renderer project
  | SomeAnalysisParser parser prelude <- someAnalysisParser
    (Proxy :: Proxy '[ Analysis.Evaluatable, Analysis.Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) (projectLanguage project) = do
    package <- parsePackage parser prelude project
    let graph = case graphType of
          ImportGraph -> graphImports
          CallGraph   -> graphCalls
    graph package >>= case renderer of
      JSONGraphRenderer   -> pure . toOutput
      DOTGraphRenderer    -> pure . renderGraph

-- | Parse a list of files into a 'Package'.
parsePackage :: Members '[Distribute WrappedTask, Files, Task] effs
             => Parser term       -- ^ A parser.
             -> Maybe File        -- ^ Prelude (optional).
             -> Project           -- ^ Project to parse into a package.
             -> Eff effs (Package term)
parsePackage parser preludeFile project@Project{..} = do
  prelude <- traverse (parseModule parser Nothing) preludeFile
  p <- parseModules parser project
  trace ("project: " <> show p) $ pure (Package.fromModules n Nothing prelude (length projectEntryPoints) p)
  where
    n = name (projectName project)

-- | Parse all files in a project into 'Module's.
parseModules :: Members '[Distribute WrappedTask, Files, Task] effs => Parser term -> Project -> Eff effs [Module term]
parseModules parser Project{..} = distributeFor (projectEntryPoints <> projectFiles) (WrapTask . parseModule parser (Just projectRootDir))


-- | Parse a list of packages from a python project.
parsePythonPackage :: (Show ann
                    , Apply Show1 syntax
                    , (Term (Union syntax) ann) ~ term
                    , Members '[Exc SomeException, Distribute WrappedTask, Files, Task] effs)
                   => Parser term       -- ^ A parser.
                   -> Maybe File        -- ^ Prelude (optional).
                   -> Project           -- ^ Project to parse into a package.
                   -> Eff effs (Package term)
parsePythonPackage parser preludeFile project@Project{..} = do
  prelude <- traverse (parseModule parser Nothing) preludeFile
  setupFile <- maybe (error "no setup.py found in project") pure (find ((== (projectRootDir </> "setup.py")) . filePath) projectFiles)
  setupModule <- parseModule parser (Just projectRootDir) setupFile
  strat <- extractStrategy setupModule
  case strat of
    Unknown -> do
      p <- parseModules parser project
      pure (Package.fromModules n Nothing prelude (length projectEntryPoints) p)
    _ -> undefined
  where
    n = name (projectName project)

extractStrategy :: ( Show ann
                   , Apply Show1 syntax
                   , Members '[Exc SomeException, Task] effs
                   )
                => Module (Term (Union syntax) ann) -> Eff effs Strategy
extractStrategy setupModule = analyze (Analysis.evaluateModule setupModule `asAnalysisForTypeOfModule` setupModule) >>= extractResult
  where
    asAnalysisForTypeOfModule :: PythonAnalysis term effs (Value Precise)
                               -> Module term
                               -> PythonAnalysis term effs (Value Precise)
    asAnalysisForTypeOfModule = const

type PythonAnalysis term
  = PythonPackaging
  ( BadAddresses
  ( BadModuleResolutions
  ( BadVariables
  ( BadValues
  ( BadSyntax
  ( Erroring (Analysis.LoadError term)
  ( Evaluating
    Precise
    term
    (Value Precise))))))))

-- Load the prelude
-- Find the setup.py file in the list of projectFiles
-- parse the setup.py module and run it through the PythonPackage analysis that returns a list of files, a call to find_packages, or an unknown..
-- If it's the list of packages, select the project files for each of those packages and construct a list of packages.
-- If it's a call to find_packages, traverse the list of directories looking for __init__.py files and evaluate those as entry points.
-- Otherwise fail with an error.
--
-- Other considerations
-- If a project has no setup.py file we may need to look for __init__.py files and evaluate those directories as packages. A project may also have standalone files in a project. Should we consider those to be entry points as well?
-- If a project has no __init__ files we should consider every file to be an entry point and the entire project to be a package.

-- | Parse a file into a 'Module'.
parseModule :: Members '[Files, Task] effs => Parser term -> Maybe FilePath -> File -> Eff effs (Module term)
parseModule parser rootDir file = do
  blob <- readBlob file
  moduleForBlob rootDir blob <$> parse parser blob

type GraphAnalysis term
  = BadAddresses
  ( BadModuleResolutions
  ( BadVariables
  ( BadValues
  ( BadSyntax
  ( Erroring (Analysis.LoadError term)
  ( Evaluating
    (Located Precise term)
    term
    (Value (Located Precise term))))))))

-- | Render the import graph for a given 'Package'.
graphImports :: ( Show ann
                , Ord ann
                , Apply Analysis.Declarations1 syntax
                , Apply Analysis.Evaluatable syntax
                , Apply FreeVariables1 syntax
                , Apply Functor syntax
                , Apply Ord1 syntax
                , Apply Eq1 syntax
                , Apply Show1 syntax
                , Members '[Exc SomeException, Task] effs
                )
             => Package (Term (Union syntax) ann) -> Eff effs Graph
graphImports package = analyze (Analysis.evaluatePackage package `asAnalysisForTypeOfPackage` package) >>= extractResult
  where
    asAnalysisForTypeOfPackage :: ImportGraphing (GraphAnalysis term) effs value
                               -> Package term
                               -> ImportGraphing (GraphAnalysis term) effs value
    asAnalysisForTypeOfPackage = const

-- | Render the call graph for a given 'Package'.
graphCalls :: ( Show ann
                , Ord ann
                , Apply Analysis.Declarations1 syntax
                , Apply Analysis.Evaluatable syntax
                , Apply FreeVariables1 syntax
                , Apply Functor syntax
                , Apply Ord1 syntax
                , Apply Eq1 syntax
                , Apply Show1 syntax
                , Member Syntax.Identifier syntax
                , Members '[Exc SomeException, Task] effs
                )
             => Package (Term (Union syntax) ann) -> Eff effs Graph
graphCalls package = analyze (Analysis.evaluatePackage package `asAnalysisForTypeOfPackage` package) >>= extractResult
  where
    asAnalysisForTypeOfPackage :: CallGraphing (GraphAnalysis term) effs value
                               -> Package term
                               -> CallGraphing (GraphAnalysis term) effs value
    asAnalysisForTypeOfPackage = const

extractResult :: (Show a, Show b, Show c, Show result, Show err, Show aux, Member (Exc SomeException) e)
             => (Either err (Either a ((b, result), c)), aux)
             -> Eff e result
extractResult result = case result of
  (Right (Right ((_, graph), _)), _) -> pure graph
  err -> throwError (toException (Exc.ErrorCall ("extractResult: graph rendering failed " <> show err)))
