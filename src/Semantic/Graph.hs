{-# LANGUAGE GADTs, TypeOperators #-}
module Semantic.Graph where

import           Analysis.Abstract.Evaluating
import           Analysis.Abstract.Graph
import           Control.Effect (runIgnoringTraces)
import qualified Control.Exception as Exc
import           Control.Monad.Effect (relayState)
import           Control.Monad.Effect.Trace (trace)
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

graph :: Members '[Distribute WrappedTask, Files, Task, Exc SomeException, Telemetry, Trace] effs
      => GraphType
      -> GraphRenderer output
      -> Project
      -> Eff effs ByteString
graph graphType renderer project
  | SomeAnalysisParser parser prelude <- someAnalysisParser
    (Proxy :: Proxy '[ Evaluatable, Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) (projectLanguage project) = do
    package <- parsePackage parser prelude project
    let perTerm = case graphType of
          ImportGraph -> graphingLoadErrors
          CallGraph   -> graphingLoadErrors . graphingTerms
    analyze runGraphAnalysis (evaluatePackageWith graphingModules perTerm package) >>= extractGraph >>= case renderer of
      JSONGraphRenderer -> pure . toOutput
      DOTGraphRenderer  -> pure . renderGraph
    where extractGraph result = case result of
            (Right ((_, graph), _), _) -> pure graph
            _ -> Task.throwError (toException (Exc.ErrorCall ("graphImports: import graph rendering failed " <> show result)))
          runGraphAnalysis
            = run
            . evaluating
            . runIgnoringTraces
            . resumingLoadError
            . resumingUnspecialized
            . resumingValueError
            . resumingEnvironmentError
            . resumingEvalError
            . resumingResolutionError
            . resumingAddressError
            . graphing
            . constrainingTypes

          constrainingTypes :: Evaluator (Located Precise) term (Value (Located Precise)) effects a -> Evaluator (Located Precise) term (Value (Located Precise)) effects a
          constrainingTypes = id

-- | Parse a list of files into a 'Package'.
parsePackage :: Members '[Distribute WrappedTask, Files, Task, Trace] effs
             => Parser term -- ^ A parser.
             -> Maybe File  -- ^ Prelude (optional).
             -> Project     -- ^ Project to parse into a package.
             -> Eff effs (Package term)
parsePackage parser preludeFile project@Project{..} = do
  prelude <- traverse (parseModule parser Nothing) preludeFile
  p <- parseModules parser project
  let pkg = Package.fromModules n Nothing prelude (length projectEntryPoints) p
  pkg <$ trace ("project: " <> show pkg)

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


resumingResolutionError :: (Applicative (m effects), Effectful m, Member Trace effects) => m (Resumable ResolutionError ': effects) a -> m effects a
resumingResolutionError = runResolutionErrorWith (\ err -> raise (trace ("ResolutionError:" <> show err)) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: Evaluator location term value (Resumable (LoadError term) ': effects) a -> Evaluator location term value effects a
resumingLoadError = runLoadErrorWith (\ (LoadError _) -> pure [])

resumingEvalError :: (AbstractHole value, Show value) => Evaluator location term value (Resumable (EvalError value) ': effects) a -> Evaluator location term value effects a
resumingEvalError = runEvalErrorWith (\ err -> traceM ("EvalError" <> show err) *> case err of
  EnvironmentLookupError{} -> pure hole
  DefaultExportError{}     -> pure ()
  ExportError{}            -> pure ()
  IntegerFormatError{}     -> pure 0
  FloatFormatError{}       -> pure 0
  RationalFormatError{}    -> pure 0
  FreeVariablesError names -> pure (fromMaybeLast "unknown" names))

resumingUnspecialized :: (Member Trace effects, AbstractHole value) => Evaluator location term value (Resumable (Unspecialized value) ': effects) a -> Evaluator location term value effects a
resumingUnspecialized = runUnspecializedWith (\ err@(Unspecialized _) -> traceE ("Unspecialized:" <> show err) $> hole)

resumingAddressError :: (AbstractHole value, Lower (Cell location value), Member Trace effects, Show location) => Evaluator location term value (Resumable (AddressError location value) ': effects) a -> Evaluator location term value effects a
resumingAddressError = runAddressErrorWith (\ err -> traceE ("AddressError:" <> show err) *> case err of
  UnallocatedAddress _   -> pure lowerBound
  UninitializedAddress _ -> pure hole)

resumingValueError :: (AbstractHole value, Member (State (Environment location value)) effects, Member Trace effects, Show value) => Evaluator location term value (Resumable (ValueError location value) ': effects) a -> Evaluator location term value effects a
resumingValueError = runValueErrorWith (\ err -> traceE ("ValueError" <> show err) *> case err of
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

resumingEnvironmentError :: AbstractHole value => Evaluator location term value (Resumable (EnvironmentError value) ': effects) a -> Evaluator location term value effects (a, [Name])
resumingEnvironmentError = raiseHandler (relayState [] (fmap pure . flip (,)) (\ names (Resumable (FreeVariable name)) yield -> yield (name : names) hole))
