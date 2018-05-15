{-# LANGUAGE GADTs, TypeOperators #-}
module Semantic.Graph
( graph
, GraphType(..)
, Graph
, Vertex
, style
, parsePackage
, withTermSpans
, resumingResolutionError
, resumingLoadError
, resumingEvalError
, resumingUnspecialized
, resumingAddressError
, resumingValueError
, resumingEnvironmentError
) where

import           Analysis.Abstract.Evaluating
import           Analysis.Abstract.Graph
import           Control.Monad.Effect.Trace
import qualified Control.Exception as Exc
import           Control.Monad.Effect (reinterpret)
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import           Data.Abstract.Package as Package
import           Data.Abstract.Value (Value, ValueError(..), runValueErrorWith)
import           Data.ByteString.Char8 (pack)
import           Data.File
import           Data.Record
import           Data.Semilattice.Lower
import           Data.Term
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Semantic.IO (Files)
import           Semantic.Task as Task

data GraphType = ImportGraph | CallGraph

graph :: Members '[Distribute WrappedTask, Files, Task, Exc SomeException, Telemetry, Trace] effs
      => GraphType
      -> Project
      -> Eff effs (Graph Vertex)
graph graphType project
  | SomeAnalysisParser parser prelude <- someAnalysisParser
    (Proxy :: Proxy '[ Evaluatable, Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) (projectLanguage project) = do
    package <- parsePackage parser prelude project
    let analyzeTerm = case graphType of
          ImportGraph -> id
          CallGraph   -> graphingTerms
    analyze runGraphAnalysis (evaluatePackageWith graphingModules (withTermSpans . graphingLoadErrors . analyzeTerm) package) >>= extractGraph
    where extractGraph result = case result of
            (Right ((_, graph), _), _) -> pure graph
            _ -> Task.throwError (toException (Exc.ErrorCall ("graphImports: import graph rendering failed " <> show result)))
          runGraphAnalysis
            = run
            . evaluating
            . runIgnoringTrace
            . resumingLoadError
            . resumingUnspecialized
            . resumingValueError
            . resumingEnvironmentError
            . resumingEvalError
            . resumingResolutionError
            . resumingAddressError
            . graphing
            . constrainingTypes

          constrainingTypes :: Evaluator (Located Precise) (Value (Located Precise)) effects a -> Evaluator (Located Precise) (Value (Located Precise)) effects a
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


withTermSpans :: ( HasField fields Span
                 , Member (Reader Span) effects
                 )
              => SubtermAlgebra (TermF syntax (Record fields)) (Term syntax (Record fields)) (Evaluator location value effects a)
              -> SubtermAlgebra (TermF syntax (Record fields)) (Term syntax (Record fields)) (Evaluator location value effects a)
withTermSpans recur term = withCurrentSpan (getField (termFAnnotation term)) (recur term)

resumingResolutionError :: (Applicative (m effects), Effectful m, Member Trace effects) => m (Resumable ResolutionError ': effects) a -> m effects a
resumingResolutionError = runResolutionErrorWith (\ err -> trace ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: Member Trace effects => Evaluator location value (Resumable (LoadError location value) ': effects) a -> Evaluator location value effects a
resumingLoadError = runLoadErrorWith (\ (ModuleNotFound path) -> trace ("LoadError: " <> path) $> Nothing)

resumingEvalError :: (AbstractHole value, Member Trace effects, Show value) => Evaluator location value (Resumable (EvalError value) ': effects) a -> Evaluator location value effects a
resumingEvalError = runEvalErrorWith (\ err -> trace ("EvalError" <> show err) *> case err of
  EnvironmentLookupError{} -> pure hole
  DefaultExportError{}     -> pure ()
  ExportError{}            -> pure ()
  IntegerFormatError{}     -> pure 0
  FloatFormatError{}       -> pure 0
  RationalFormatError{}    -> pure 0
  FreeVariablesError names -> pure (fromMaybeLast "unknown" names))

resumingUnspecialized :: (Member Trace effects, AbstractHole value) => Evaluator location value (Resumable (Unspecialized value) ': effects) a -> Evaluator location value effects a
resumingUnspecialized = runUnspecializedWith (\ err@(Unspecialized _) -> trace ("Unspecialized:" <> show err) $> hole)

resumingAddressError :: (AbstractHole value, Lower (Cell location value), Member Trace effects, Show location) => Evaluator location value (Resumable (AddressError location value) ': effects) a -> Evaluator location value effects a
resumingAddressError = runAddressErrorWith (\ err -> trace ("AddressError:" <> show err) *> case err of
  UnallocatedAddress _   -> pure lowerBound
  UninitializedAddress _ -> pure hole)

resumingValueError :: (Members '[State (Environment location (Value location)), Trace] effects, Show location) => Evaluator location (Value location) (Resumable (ValueError location) ': effects) a -> Evaluator location (Value location) effects a
resumingValueError = runValueErrorWith (\ err -> trace ("ValueError" <> show err) *> case err of
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

resumingEnvironmentError :: AbstractHole value => Evaluator location value (Resumable (EnvironmentError value) ': effects) a -> Evaluator location value effects (a, [Name])
resumingEnvironmentError
  = runState []
  . reinterpret (\ (Resumable (FreeVariable name)) -> modify' (name :) $> hole)
