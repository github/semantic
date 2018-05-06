{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators #-}
module Semantic.Graph where

import           Analysis.Abstract.Evaluating
import           Analysis.Abstract.ImportGraph
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
import qualified Data.Syntax as Syntax
import           Data.Term
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Rendering.Renderer
import           Semantic.IO (Files)
import           Semantic.Task as Task

graph :: Members '[Distribute WrappedTask, Files, Task, Exc SomeException, Telemetry] effs
      => GraphRenderer output
      -> Project
      -> Eff effs ByteString
graph renderer project
  | SomeAnalysisParser parser prelude <- someAnalysisParser
    (Proxy :: Proxy '[ Evaluatable, Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) (projectLanguage project) = do
    parsePackage parser prelude project >>= graphImports >>= case renderer of
      JSONGraphRenderer -> pure . toOutput
      DOTGraphRenderer  -> pure . renderImportGraph

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

-- | Parse a file into a 'Module'.
parseModule :: Members '[Files, Task] effs => Parser term -> Maybe FilePath -> File -> Eff effs (Module term)
parseModule parser rootDir file = do
  blob <- readBlob file
  moduleForBlob rootDir blob <$> parse parser blob


importGraphAnalysis :: forall term syntax ann a
                    .  Evaluator (Located Precise) term (Value (Located Precise))
                       '[ State (ImportGraph (Term (Sum syntax) ann))
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
                    -> (   Either String                                                   -- 'fail' calls
                         ( Either (SomeExc (LoadError term))                               -- Unhandled LoadErrors
                         ( ( a                                                             -- the result value
                           , ImportGraph (Term (Sum syntax) ann))                          -- the import graph
                         , [Name]))                                                        -- the list of bad names
                       , EvaluatingState (Located Precise) term (Value (Located Precise))) -- the final state
importGraphAnalysis
  = run
  . evaluating
  . runLoadError
  . resumingUnspecialized
  . resumingValueError
  . resumingEvalError
  . resumingResolutionError
  . resumingAddressError
  . importGraphing

resumingResolutionError :: (Applicative (m effects), Effectful m) => m (Resumable ResolutionError ': effects) a -> m effects a
resumingResolutionError = runResolutionErrorWith (\ err -> traceM ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

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
    FreeVariableError name   -> raise (modify' (name :)) *> pure hole
    FreeVariablesError names -> raise (modify' (names <>)) *> pure (fromMaybeLast "unknown" names))

resumingUnspecialized :: AbstractHole value => Evaluator location term value (Resumable (Unspecialized value) ': effects) a -> Evaluator location term value effects a
resumingUnspecialized = runUnspecializedWith (\ err@(Unspecialized _) -> traceM ("Unspecialized:" <> show err) *> pure hole)

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

-- | Render the import graph for a given 'Package'.
graphImports :: ( Show ann
                , Apply Declarations1 syntax
                , Apply Evaluatable syntax
                , Apply FreeVariables1 syntax
                , Apply Functor syntax
                , Apply Show1 syntax
                , Element Syntax.Identifier syntax
                , Members '[Exc SomeException, Task] effs
                )
             => Package (Term (Sum syntax) ann)
             -> Eff effs (ImportGraph (Term (Sum syntax) ann))
graphImports package = analyze importGraphAnalysis (evaluatePackageWith graphingModules (graphingLoadErrors . graphingTerms) package) >>= extractGraph
  where
    extractGraph result = case result of
      (Right (Right ((_, graph), _)), _) -> pure graph
      _ -> Task.throwError (toException (Exc.ErrorCall ("graphImports: import graph rendering failed " <> show result)))
