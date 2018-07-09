{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators #-}
module Semantic.Graph
( runGraph
, runCallGraph
, runImportGraph
, GraphType(..)
, Graph
, Vertex
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


import Prelude hiding (readFile)

import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Graph as Graph
import           Control.Abstract
import           Control.Monad.Effect (reinterpret)
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package as Package
import           Data.Abstract.Value.Type
import           Data.Abstract.Value.Concrete (Value, ValueError (..), runValueErrorWith)
import           Data.Graph
import           Data.Project
import           Data.Record
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.Text (pack)
import           Parsing.Parser
import           Prologue hiding (MonadError (..), TypeError (..))
import           Semantic.Task as Task

data GraphType = ImportGraph | CallGraph

type AnalysisClasses = '[ Declarations1, Eq1, Evaluatable, FreeVariables1, Foldable, Functor, Ord1, Show1 ]

runGraph :: forall effs. (Member Distribute effs, Member (Exc SomeException) effs, Member Resolution effs, Member Task effs, Member Trace effs, Effects effs)
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
    modules <- topologicalSort <$> runImportGraph lang package
    runCallGraph lang includePackages modules package

runCallGraph :: ( HasField ann Span
                , Element Syntax.Identifier syntax
                , Base term ~ TermF (Sum syntax) (Record ann)
                , Ord term
                , Corecursive term
                , Declarations term
                , Evaluatable (Base term)
                , FreeVariables term
                , HasPrelude lang
                , Member Trace effs
                , Recursive term
                , Effects effs
                )
             => Proxy lang
             -> Bool
             -> [Module term]
             -> Package term
             -> Eff effs (Graph Vertex)
runCallGraph lang includePackages modules package = do
  let analyzeTerm = withTermSpans . graphingTerms . cachingTerms
      analyzeModule = (if includePackages then graphingPackages else id) . convergingModules . graphingModules
      extractGraph (_, (_, (graph, _))) = simplify graph
      runGraphAnalysis
        = runState (lowerBound @(Heap (Hole (Located Monovariant)) All Type))
        . runFresh 0
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . runTermEvaluator @_ @(Hole (Located Monovariant)) @Type
        . graphing
        . caching @[]
        . resumingTypeError
        . runReader (packageInfo package)
        . runReader (lowerBound @Span)
        . providingLiveSet
        . runReader (lowerBound @(ModuleTable (NonEmpty (Module (Environment (Hole (Located Monovariant)), Hole (Located Monovariant))))))
        . raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
  extractGraph <$> runEvaluator (runGraphAnalysis (evaluate lang analyzeModule analyzeTerm modules))



runImportGraph :: forall effs lang term.
                  ( Declarations term
                  , Evaluatable (Base term)
                  , FreeVariables term
                  , HasPrelude lang
                  , Member Trace effs
                  , Recursive term
                  , Effects effs
                  )
               => Proxy lang
               -> Package term
               -> Eff effs (Graph (Module term))
runImportGraph lang (package :: Package term)
  -- Optimization for the common (when debugging) case of one-and-only-one module.
  | [m :| []] <- toList (packageModules package) = vertex m <$ trace ("single module, skipping import graph computation for " <> modulePath (moduleInfo m))
  | otherwise =
  let analyzeModule = graphingModuleInfo
      extractGraph (_, (_, (graph, _))) = do
        info <- graph
        maybe lowerBound (foldMap vertex) (ModuleTable.lookup (modulePath info) (packageModules package))
      runImportGraphAnalysis
        = runState lowerBound
        . runFresh 0
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . resumingValueError
        . runState lowerBound
        . runReader lowerBound
        . runModules (ModuleTable.modulePaths (packageModules package))
        . runTermEvaluator @_ @_ @(Value (Hole Precise) (ImportGraphEff term (Hole Precise) effs))
        . runReader (packageInfo package)
        . runReader lowerBound
  in extractGraph <$> runEvaluator (runImportGraphAnalysis (evaluate @_ @_ @_ @_ @term lang analyzeModule id (ModuleTable.toPairs (packageModules package) >>= toList . snd)))

newtype ImportGraphEff term address outerEffects a = ImportGraphEff
  { runImportGraphEff :: Eff (  Exc (LoopControl address)
                             ': Exc (Return address)
                             ': Env address
                             ': Allocator address (Value address (ImportGraphEff term address outerEffects))
                             ': Reader ModuleInfo
                             ': Reader Span
                             ': Reader PackageInfo
                             ': Modules address
                             ': Reader (ModuleTable (NonEmpty (Module (Environment address, address))))
                             ': State (Graph ModuleInfo)
                             ': Resumable (ValueError address (ImportGraphEff term address outerEffects))
                             ': Resumable (AddressError address (Value address (ImportGraphEff term address outerEffects)))
                             ': Resumable ResolutionError
                             ': Resumable EvalError
                             ': Resumable (EnvironmentError address)
                             ': Resumable (Unspecialized (Value address (ImportGraphEff term address outerEffects)))
                             ': Resumable (LoadError address)
                             ': Fresh
                             ': State (Heap address Latest (Value address (ImportGraphEff term address outerEffects)))
                             ': outerEffects
                             ) a
  }


-- | Parse a list of files into a 'Package'.
parsePackage :: (Member Distribute effs, Member (Exc SomeException) effs, Member Resolution effs, Member Task effs, Member Trace effs)
             => Parser term -- ^ A parser.
             -> Project     -- ^ Project to parse into a package.
             -> Eff effs (Package term)
parsePackage parser project@Project{..} = do
  p <- parseModules parser project
  resMap <- Task.resolutionMap project
  let pkg = Package.fromModules n p resMap
  pkg <$ trace ("project: " <> show (() <$ pkg))

  where
    n = name (projectName project)

    -- | Parse all files in a project into 'Module's.
    parseModules :: (Member Distribute effs, Member (Exc SomeException) effs, Member Task effs) => Parser term -> Project -> Eff effs [Module term]
    parseModules parser p@Project{..} = distributeFor (projectFiles p) (parseModule p parser)

-- | Parse a file into a 'Module'.
parseModule :: (Member (Exc SomeException) effs, Member Task effs) => Project -> Parser term -> File -> Eff effs (Module term)
parseModule proj parser file = do
  mBlob <- readFile proj file
  case mBlob of
    Just blob -> moduleForBlob (Just (projectRootDir proj)) blob <$> parse parser blob
    Nothing   -> throwError (SomeException (FileNotFound (filePath file)))

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

resumingLoadError :: (Member Trace effects, AbstractHole address, Effects effects) => Evaluator address value (Resumable (LoadError address) ': effects) a -> Evaluator address value effects a
resumingLoadError = runLoadErrorWith (\ (ModuleNotFound path) -> trace ("LoadError: " <> path) $> (lowerBound, hole))

resumingEvalError :: (Member Trace effects, Effects effects) => Evaluator address value (Resumable EvalError ': effects) a -> Evaluator address value effects a
resumingEvalError = runEvalErrorWith (\ err -> trace ("EvalError" <> show err) *> case err of
  DefaultExportError{}     -> pure ()
  ExportError{}            -> pure ()
  IntegerFormatError{}     -> pure 0
  FloatFormatError{}       -> pure 0
  RationalFormatError{}    -> pure 0
  FreeVariablesError names -> pure (fromMaybeLast (name "unknown") names))

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

resumingTypeError :: ( Alternative (m address Type (State TypeMap ': effects))
                     , Effects effects
                     , Effectful (m address Type)
                     , Member Trace effects
                     )
                  => m address Type (Resumable TypeError ': State TypeMap ': effects) a
                  -> m address Type effects a
resumingTypeError = runTypesWith (\err -> trace ("TypeError " <> show err) *> case err of
  UnificationError l r -> pure l <|> pure r
  InfiniteType _ r -> pure r)
