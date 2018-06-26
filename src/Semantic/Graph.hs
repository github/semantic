{-# LANGUAGE GADTs, TypeOperators #-}
module Semantic.Graph
( runGraph
, GraphType(..)
, Graph
, Vertex
, GraphEff(..)
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
import           Control.Abstract
import           Control.Monad.Effect (reinterpret)
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import           Data.Abstract.Package as Package
import           Data.Abstract.Value (Value, ValueError (..), runValueErrorWith)
import           Data.Graph
import           Data.Project
import           Data.Record
import           Data.Term
import           Data.Text (pack)
import           Parsing.Parser
import           Prologue hiding (MonadError (..))
import           Semantic.IO (Files)
import           Semantic.Task as Task

data GraphType = ImportGraph | CallGraph

runGraph :: (Member Distribute effs, Member Files effs, Member Resolution effs, Member Task effs, Member Trace effs)
         => GraphType
         -> Bool
         -> Project
         -> Eff effs (Graph Vertex)
runGraph graphType includePackages project
  | SomeAnalysisParser parser lang <- someAnalysisParser
    (Proxy :: Proxy '[ Evaluatable, Declarations1, FreeVariables1, Functor, Eq1, Ord1, Show1 ]) (projectLanguage project) = do
    package <- parsePackage parser project
    let analyzeTerm = withTermSpans . case graphType of
          ImportGraph -> id
          CallGraph   -> graphingTerms
        analyzeModule = (if includePackages then graphingPackages else id) . graphingModules
    analyze runGraphAnalysis (evaluatePackageWith lang analyzeModule analyzeTerm package) >>= extractGraph
    where extractGraph result = case result of
            (_, (_, (graph, _))) -> pure (simplify graph)
          runGraphAnalysis
            = run
            . evaluating
            . runIgnoringTrace
            . resumingLoadError
            . resumingUnspecialized
            . resumingEnvironmentError
            . resumingEvalError
            . resumingResolutionError
            . resumingAddressError
            . resumingValueError
            . runTermEvaluator @_ @_ @(Value (Hole (Located Precise)) (GraphEff _))
            . graphing

-- | The full list of effects in flight during the evaluation of terms. This, and other @newtype@s like it, are necessary to type 'Value', since the bodies of closures embed evaluators. This would otherwise require cycles in the effect list (i.e. references to @effects@ within @effects@ itself), which the typechecker forbids.
newtype GraphEff address a = GraphEff
  { runGraphEff :: Eff '[ LoopControl address
                        , Return address
                        , Env address
                        , Allocator address (Value address (GraphEff address))
                        , Reader ModuleInfo
                        , Modules address (Value address (GraphEff address))
                        , Reader Span
                        , Reader PackageInfo
                        , State (Graph Vertex)
                        , Resumable (ValueError address (GraphEff address))
                        , Resumable (AddressError address (Value address (GraphEff address)))
                        , Resumable ResolutionError
                        , Resumable EvalError
                        , Resumable (EnvironmentError address)
                        , Resumable (Unspecialized (Value address (GraphEff address)))
                        , Resumable (LoadError address (Value address (GraphEff address)))
                        , Trace
                        , Fresh
                        , State (Heap address Latest (Value address (GraphEff address)))
                        , State (ModuleTable (Maybe (Environment address, address)))
                        ] a
  }

-- | Parse a list of files into a 'Package'.
parsePackage :: (Member Distribute effs, Member Files effs, Member Resolution effs, Member Task effs, Member Trace effs)
             => Parser term -- ^ A parser.
             -> Project     -- ^ Project to parse into a package.
             -> Eff effs (Package term)
parsePackage parser project@Project{..} = do
  p <- parseModules parser project
  resMap <- Task.resolutionMap project
  let pkg = Package.fromModules n Nothing (length projectEntryPoints) p resMap
  pkg <$ trace ("project: " <> show pkg)

  where
    n = name (projectName project)

    -- | Parse all files in a project into 'Module's.
    parseModules :: (Member Distribute effs, Member Files effs, Member Task effs) => Parser term -> Project -> Eff effs [Module term]
    parseModules parser Project{..} = distributeFor (projectEntryPoints <> projectFiles) (parseModule parser (Just projectRootDir))

-- | Parse a file into a 'Module'.
parseModule :: (Member Files effs, Member Task effs) => Parser term -> Maybe FilePath -> File -> Eff effs (Module term)
parseModule parser rootDir file = do
  blob <- readBlob file
  moduleForBlob rootDir blob <$> parse parser blob


withTermSpans :: ( HasField fields Span
                 , Member (Reader Span) effects
                 )
              => SubtermAlgebra (TermF syntax (Record fields)) term (TermEvaluator term address value effects a)
              -> SubtermAlgebra (TermF syntax (Record fields)) term (TermEvaluator term address value effects a)
withTermSpans recur term = withCurrentSpan (getField (termFAnnotation term)) (recur term)

resumingResolutionError :: (Applicative (m effects), Effectful m, Member Trace effects, Effects effects) => m (Resumable ResolutionError ': effects) a -> m effects a
resumingResolutionError = runResolutionErrorWith (\ err -> trace ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: (Member Trace effects, Effects effects) => Evaluator address value (Resumable (LoadError address value) ': effects) a -> Evaluator address value effects a
resumingLoadError = runLoadErrorWith (\ (ModuleNotFound path) -> trace ("LoadError: " <> path) $> Nothing)

resumingEvalError :: (Member Trace effects, Effects effects) => Evaluator address value (Resumable EvalError ': effects) a -> Evaluator address value effects a
resumingEvalError = runEvalErrorWith (\ err -> trace ("EvalError" <> show err) *> case err of
  DefaultExportError{}     -> pure ()
  ExportError{}            -> pure ()
  IntegerFormatError{}     -> pure 0
  FloatFormatError{}       -> pure 0
  RationalFormatError{}    -> pure 0
  FreeVariablesError names -> pure (fromMaybeLast "unknown" names))

resumingUnspecialized :: (Member Trace effects, AbstractHole value, Effects effects) => Evaluator address value (Resumable (Unspecialized value) ': effects) a -> Evaluator address value effects a
resumingUnspecialized = runUnspecializedWith (\ err@(Unspecialized _) -> trace ("Unspecialized:" <> show err) $> hole)

resumingAddressError :: (AbstractHole value, Lower (Cell address value), Member Trace effects, Show address, Effects effects) => Evaluator address value (Resumable (AddressError address value) ': effects) a -> Evaluator address value effects a
resumingAddressError = runAddressErrorWith (\ err -> trace ("AddressError:" <> show err) *> case err of
  UnallocatedAddress _   -> pure lowerBound
  UninitializedAddress _ -> pure hole)

resumingValueError :: (Member Trace effects, Show address, Effects effects) => Evaluator address (Value address body) (Resumable (ValueError address body) ': effects) a -> Evaluator address (Value address body) effects a
resumingValueError = runValueErrorWith (\ err -> trace ("ValueError" <> show err) *> case err of
  CallError val     -> pure val
  StringError val   -> pure (pack (show val))
  BoolError{}       -> pure True
  BoundsError{}     -> pure hole
  IndexError{}      -> pure hole
  NumericError{}    -> pure hole
  Numeric2Error{}   -> pure hole
  ComparisonError{} -> pure hole
  NamespaceError{}  -> pure lowerBound
  BitwiseError{}    -> pure hole
  Bitwise2Error{}   -> pure hole
  KeyValueError{}   -> pure (hole, hole)
  ArithmeticError{} -> pure hole)

resumingEnvironmentError :: (AbstractHole address, Effects effects) => Evaluator address value (Resumable (EnvironmentError address) ': effects) a -> Evaluator address value effects ([Name], a)
resumingEnvironmentError
  = runState []
  . reinterpret (\ (Resumable (FreeVariable name)) -> modify' (name :) $> hole)
