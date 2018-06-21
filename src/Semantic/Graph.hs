{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators #-}
module Semantic.Graph
( runGraph
, runImportGraph
, GraphType(..)
, Graph
, Vertex
, GraphEff(..)
, ImportGraphEff(..)
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

import           Analysis.Abstract.Graph as Graph
import           Control.Abstract
import           Control.Monad.Effect (reinterpret)
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
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

type AnalysisClasses = '[ Declarations1, Eq1, Evaluatable, FreeVariables1, Functor, Ord1, Show1 ]

runGraph :: ( Member (Distribute WrappedTask) effs, Member Resolution effs, Member Task effs, Member Trace effs)
         => GraphType
         -> Bool
         -> Project
         -> Eff effs (Graph Vertex)
runGraph ImportGraph _ project
  | SomeAnalysisParser parser lang <- someAnalysisParser (Proxy :: Proxy AnalysisClasses) (projectLanguage project) = do
    package <- parsePackage parser project
    fmap (Graph.moduleVertex . moduleInfo) <$> runImportGraph lang package
runGraph CallGraph includePackages project
  | SomeAnalysisParser parser lang <- someAnalysisParser (Proxy :: Proxy AnalysisClasses) (projectLanguage project) = do
    package <- parsePackage parser project
    modules <- runImportGraph lang package
    let analyzeTerm = withTermSpans . graphingTerms
        analyzeModule = (if includePackages then graphingPackages else id) . graphingModules
        extractGraph (((_, graph), _), _) = simplify graph
        runGraphAnalysis
          = run
          . runState lowerBound
          . runFresh 0
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
          . runReader (packageInfo package)
          . runReader lowerBound
          . runReader lowerBound
          . raiseHandler (interpret (handleModules (ModuleTable.modulePaths (packageModules (packageBody package)))))
    extractGraph <$> analyze runGraphAnalysis (evaluate lang analyzeModule analyzeTerm (topologicalSort modules))

-- | The full list of effects in flight during the evaluation of terms. This, and other @newtype@s like it, are necessary to type 'Value', since the bodies of closures embed evaluators. This would otherwise require cycles in the effect list (i.e. references to @effects@ within @effects@ itself), which the typechecker forbids.
newtype GraphEff address a = GraphEff
  { runGraphEff :: Eff '[ LoopControl address
                        , Return address
                        , Env address
                        , Allocator address (Value address (GraphEff address))
                        , Reader ModuleInfo
                        , Modules address
                        , Reader (ModuleTable (NonEmpty (Module (address, Environment address))))
                        , Reader Span
                        , Reader PackageInfo
                        , State (Graph Vertex)
                        , Resumable (ValueError address (GraphEff address))
                        , Resumable (AddressError address (Value address (GraphEff address)))
                        , Resumable ResolutionError
                        , Resumable EvalError
                        , Resumable (EnvironmentError address)
                        , Resumable (Unspecialized (Value address (GraphEff address)))
                        , Resumable (LoadError address)
                        , Trace
                        , Fresh
                        , State (Heap address Latest (Value address (GraphEff address)))
                        ] a
  }


runImportGraph :: ( Declarations term
                  , Evaluatable (Base term)
                  , FreeVariables term
                  , HasPrelude lang
                  , Member Task effs
                  , Recursive term
                  )
               => Proxy lang
               -> Package term
               -> Eff effs (Graph (Module term))
runImportGraph lang (package :: Package term) = do
  let analyzeTerm = id
      analyzeModule = graphingModuleInfo
      extractGraph (((_, graph), _), _) = do
        info <- graph
        case ModuleTable.lookup (modulePath info) (packageModules (packageBody package)) of
          Nothing -> lowerBound
          Just m -> foldMapA pure m
      runImportGraphAnalysis
        = run
        . runState lowerBound
        . runFresh 0
        . runIgnoringTrace
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . resumingValueError
        . runState lowerBound
        . runReader lowerBound
        . interpret (handleModules (ModuleTable.modulePaths (packageModules (packageBody package))))
        . runTermEvaluator @_ @_ @(Value (Hole Precise) (ImportGraphEff term (Hole Precise)))
        . runReader (packageInfo package)
        . runReader lowerBound
  extractGraph <$> analyze runImportGraphAnalysis (evaluate @_ @_ @_ @_ @term lang analyzeModule analyzeTerm (map snd (ModuleTable.toPairs (packageModules (packageBody package)))))

newtype ImportGraphEff term address a = ImportGraphEff
  { runImportGraphEff :: Eff '[ LoopControl address
                              , Return address
                              , Env address
                              , Allocator address (Value address (ImportGraphEff term address))
                              , Reader ModuleInfo
                              , Reader Span
                              , Reader PackageInfo
                              , Modules address
                              , Reader (ModuleTable (NonEmpty (Module (address, Environment address))))
                              , State (Graph ModuleInfo)
                              , Resumable (ValueError address (ImportGraphEff term address))
                              , Resumable (AddressError address (Value address (ImportGraphEff term address)))
                              , Resumable ResolutionError
                              , Resumable EvalError
                              , Resumable (EnvironmentError address)
                              , Resumable (Unspecialized (Value address (ImportGraphEff term address)))
                              , Resumable (LoadError address)
                              , Trace
                              , Fresh
                              , State (Heap address Latest (Value address (ImportGraphEff term address)))
                              ] a
  }


-- | Parse a list of files into a 'Package'.
parsePackage :: (Member (Distribute WrappedTask) effs, Member Resolution effs, Member Trace effs)
             => Parser term -- ^ A parser.
             -> Project     -- ^ Project to parse into a package.
             -> Eff effs (Package term)
parsePackage parser project@Project{..} = do
  p <- parseModules parser project
  resMap <- Task.resolutionMap project
  let pkg = Package.fromModules n Nothing p resMap
  pkg <$ trace ("project: " <> show pkg)

  where
    n = name (projectName project)

    -- | Parse all files in a project into 'Module's.
    parseModules :: Member (Distribute WrappedTask) effs => Parser term -> Project -> Eff effs [Module term]
    parseModules parser Project{..} = distributeFor (projectEntryPoints <> projectFiles) (WrapTask . parseModule parser (Just projectRootDir))

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

resumingResolutionError :: (Applicative (m effects), Effectful m, Member Trace effects) => m (Resumable ResolutionError ': effects) a -> m effects a
resumingResolutionError = runResolutionErrorWith (\ err -> trace ("ResolutionError:" <> show err) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: Member Trace effects => Evaluator address value (Resumable (LoadError address) ': effects) a -> Evaluator address value effects a
resumingLoadError = runLoadErrorWith (\ (ModuleNotFound path) -> trace ("LoadError: " <> path) $> Nothing)

resumingEvalError :: Member Trace effects => Evaluator address value (Resumable EvalError ': effects) a -> Evaluator address value effects a
resumingEvalError = runEvalErrorWith (\ err -> trace ("EvalError" <> show err) *> case err of
  DefaultExportError{}     -> pure ()
  ExportError{}            -> pure ()
  IntegerFormatError{}     -> pure 0
  FloatFormatError{}       -> pure 0
  RationalFormatError{}    -> pure 0
  FreeVariablesError names -> pure (fromMaybeLast "unknown" names))

resumingUnspecialized :: (Member Trace effects, AbstractHole value) => Evaluator address value (Resumable (Unspecialized value) ': effects) a -> Evaluator address value effects a
resumingUnspecialized = runUnspecializedWith (\ err@(Unspecialized _) -> trace ("Unspecialized:" <> show err) $> hole)

resumingAddressError :: (AbstractHole value, Lower (Cell address value), Member Trace effects, Show address) => Evaluator address value (Resumable (AddressError address value) ': effects) a -> Evaluator address value effects a
resumingAddressError = runAddressErrorWith (\ err -> trace ("AddressError:" <> show err) *> case err of
  UnallocatedAddress _   -> pure lowerBound
  UninitializedAddress _ -> pure hole)

resumingValueError :: (Member Trace effects, Show address) => Evaluator address (Value address body) (Resumable (ValueError address body) ': effects) a -> Evaluator address (Value address body) effects a
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

resumingEnvironmentError :: AbstractHole address => Evaluator address value (Resumable (EnvironmentError address) ': effects) a -> Evaluator address value effects (a, [Name])
resumingEnvironmentError
  = runState []
  . reinterpret (\ (Resumable (FreeVariable name)) -> modify' (name :) $> hole)
