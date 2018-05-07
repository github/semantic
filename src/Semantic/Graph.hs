{-# LANGUAGE GADTs, TypeOperators #-}
module Semantic.Graph where

import           Analysis.Abstract.Evaluating
import           Analysis.Abstract.Graph
import qualified Control.Exception as Exc
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Located
import           Data.Abstract.Module
import           Data.Abstract.Package as Package
import           Data.Abstract.Value (Value, ValueError(..), runValueErrorWith)
import           Data.ByteString.Char8 (pack)
import           Data.File
import           Data.Output
import           Data.Semilattice.Lower
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Rendering.Renderer
import           Semantic.IO (Files)
import           Semantic.Task as Task

data GraphType = ImportGraph | CallGraph

graph :: Members '[Distribute WrappedTask, Files, Task, Exc SomeException, Telemetry] effs
      => GraphType
      -> GraphRenderer output
      -> Project
      -> Eff effs ByteString
graph graphType renderer project
  | SomeAnalysisParser parser prelude <- someAnalysisParser
    (Proxy :: Proxy '[ Evaluatable, Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) (projectLanguage project) = do
    package <- parsePackage parser prelude project
    let graph package = case graphType of
          ImportGraph -> analyze runGraphAnalysis (evaluatePackageWith graphingModules  graphingLoadErrors                  package)
          CallGraph   -> analyze runGraphAnalysis (evaluatePackageWith graphingModules (graphingLoadErrors . graphingTerms) package)
    graph package >>= extractGraph >>= case renderer of
      JSONGraphRenderer -> pure . toOutput
      DOTGraphRenderer  -> pure . renderGraph

-- | Parse a list of files into a 'Package'.
parsePackage :: Members '[Distribute WrappedTask, Files, Task] effs
             => Parser term -- ^ A parser.
             -> Maybe File  -- ^ Prelude (optional).
             -> Project     -- ^ Project to parse into a package.
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

-- | Parse a file into a 'Module'.
parseModule :: Members '[Files, Task] effs => Parser term -> Maybe FilePath -> File -> Eff effs (Module term)
parseModule parser rootDir file = do
  blob <- readBlob file
  moduleForBlob rootDir blob <$> parse parser blob


runGraphAnalysis :: Evaluator (Located Precise) term (Value (Located Precise))
                      '[ State Graph
                       , Resumable (AddressError (Located Precise) (Value (Located Precise)))
                       , Resumable ResolutionError
                       , Resumable (EvalError (Value (Located Precise)))
                       , State [Name]
                       , Resumable (ValueError (Located Precise) (Value (Located Precise)))
                       , Resumable (Unspecialized (Value (Located Precise)))
                       , Resumable (LoadError term)
                       , Fail
                       , Fresh
                       , Reader (Environment (Located Precise) (Value (Located Precise)))
                       , State (Environment (Located Precise) (Value (Located Precise)))
                       , State (Heap (Located Precise) (Value (Located Precise)))
                       , State (ModuleTable (Environment (Located Precise) (Value (Located Precise)), Value (Located Precise)))
                       , State (Exports (Located Precise) (Value (Located Precise)))
                       , State (JumpTable term)
                       ] a
                 -> ( Either String                                                     -- 'fail' calls
                      ( ( a                                                             -- the result value
                        , Graph)                                                        -- the import graph
                      , [Name])                                                         -- the list of bad names
                    , EvaluatingState (Located Precise) term (Value (Located Precise))) -- the final state
runGraphAnalysis
  = run
  . evaluating
  . resumingLoadError
  . resumingUnspecialized
  . resumingValueError
  . resumingEvalError
  . resumingResolutionError
  . resumingAddressError
  . graphing

resumingResolutionError :: (Applicative (m effects), Effectful m) => m (Resumable ResolutionError ': effects) a -> m effects a
resumingResolutionError = runResolutionErrorWith (\ err -> traceM ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: Evaluator location term value (Resumable (LoadError term) ': effects) a -> Evaluator location term value effects a
resumingLoadError = runLoadErrorWith (\ (LoadError _) -> pure [])

resumingEvalError :: (AbstractHole value, Show value) => Evaluator location term value (Resumable (EvalError value) ': State [Name] ': effects) a -> Evaluator location term value effects (a, [Name])
resumingEvalError
  = runState []
  . runEvalErrorWith (\ err -> traceM ("EvalError" <> show err) *> case err of
    EnvironmentLookupError{} -> pure hole
    DefaultExportError{}     -> pure ()
    ExportError{}            -> pure ()
    IntegerFormatError{}     -> pure 0
    FloatFormatError{}       -> pure 0
    RationalFormatError{}    -> pure 0
    FreeVariableError name   -> raise (modify' (name :)) $> hole
    FreeVariablesError names -> raise (modify' (names <>)) $> (fromMaybeLast "unknown" names))

resumingUnspecialized :: AbstractHole value => Evaluator location term value (Resumable (Unspecialized value) ': effects) a -> Evaluator location term value effects a
resumingUnspecialized = runUnspecializedWith (\ err@(Unspecialized _) -> traceM ("Unspecialized:" <> show err) $> hole)

resumingAddressError :: (AbstractHole value, Lower (Cell location value), Show location) => Evaluator location term value (Resumable (AddressError location value) ': effects) a -> Evaluator location term value effects a
resumingAddressError = runAddressErrorWith (\ err -> traceM ("AddressError:" <> show err) *> case err of
  UnallocatedAddress _   -> pure lowerBound
  UninitializedAddress _ -> pure hole)

resumingValueError :: (AbstractHole value, Member (State (Environment location value)) effects, Show value) => Evaluator location term value (Resumable (ValueError location value) ': effects) a -> Evaluator location term value effects a
resumingValueError = runValueErrorWith (\ err -> traceM ("ValueError" <> show err) *> case err of
  CallError val     -> pure val
  StringError val   -> pure (pack (show val))
  BoolError{}       -> pure True
  BoundsError{}     -> pure hole
  IndexError{}      -> pure hole
  NumericError{}    -> pure hole
  Numeric2Error{}   -> pure hole
  ComparisonError{} -> pure hole
  NamespaceError{}  -> getEnv
  BitwiseError{}    -> pure hole
  Bitwise2Error{}   -> pure hole
  KeyValueError{}   -> pure (hole, hole)
  ArithmeticError{} -> pure hole)

extractGraph :: (Member (Exc SomeException) effects, Show result, Show state) => (Either String ((result, Graph), [Name]), state) -> Eff effects Graph
extractGraph result = case result of
  (Right ((_, graph), _), _) -> pure graph
  _ -> Task.throwError (toException (Exc.ErrorCall ("graphImports: import graph rendering failed " <> show result)))
