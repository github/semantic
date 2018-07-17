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
, resumingTypeError
) where


import Prelude hiding (readFile)

import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Graph as Graph
import           Control.Abstract
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package as Package
import           Data.Abstract.Value.Abstract
import           Data.Abstract.Value.Type
import           Data.Abstract.Value.Concrete (Value, ValueError (..), runValueErrorWith)
import           Data.Graph
import           Data.Project
import           Data.Record
import qualified Data.Syntax as Syntax
import           Data.Term
import           Data.Text (pack)
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Parsing.Parser
import           Prologue hiding (MonadError (..), TypeError (..))
import           Semantic.Task as Task
import           Text.Show.Pretty (ppShow)

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
                , HasPostlude lang
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
      extractGraph (_, (_, pairs)) = simplify (foldMap fst pairs)
      runGraphAnalysis
        = runTermEvaluator @_ @(Hole (Maybe Name) (Located Monovariant)) @Abstract
        . runState (lowerBound @(Heap (Hole (Maybe Name) (Located Monovariant)) All Abstract))
        . runFresh 0
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . caching
        . graphing
        . runReader (packageInfo package)
        . runReader (lowerBound @Span)
        . providingLiveSet
        . runReader (lowerBound @(ModuleTable (NonEmpty (Module (Environment (Hole (Maybe Name) (Located Monovariant)), Hole (Maybe Name) (Located Monovariant))))))
        . raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
  extractGraph <$> runEvaluator (runGraphAnalysis (evaluate lang analyzeModule analyzeTerm modules))



runImportGraph :: forall effs lang term.
                  ( Declarations term
                  , Evaluatable (Base term)
                  , FreeVariables term
                  , HasPrelude lang
                  , HasPostlude lang
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
      extractGraph (_, (graph, _)) = do
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
        . runTermEvaluator @_ @_ @(Value (Hole (Maybe Name) Precise) (ImportGraphEff term (Hole (Maybe Name) Precise) effs))
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
  pkg <$ trace ("project: " <> prettyShow (() <$ pkg))

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
resumingResolutionError = runResolutionErrorWith (\ err -> trace ("ResolutionError: " <> prettyShow err) *> case err of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: (AbstractHole address, Effectful (m address value), Effects effects, Functor (m address value effects), Member Trace effects) => m address value (Resumable (LoadError address) ': effects) a -> m address value effects a
resumingLoadError = runLoadErrorWith (\ (ModuleNotFound path) -> trace ("LoadError: " <> path) $> (lowerBound, hole))

resumingEvalError :: (Applicative (m effects), Effectful m, Effects effects, Member Fresh effects, Member Trace effects) => m (Resumable EvalError ': effects) a -> m effects a
resumingEvalError = runEvalErrorWith (\ err -> trace ("EvalError:" <> prettyShow err) *> case err of
  DefaultExportError{}  -> pure ()
  ExportError{}         -> pure ()
  IntegerFormatError{}  -> pure 0
  FloatFormatError{}    -> pure 0
  RationalFormatError{} -> pure 0
  NoNameError           -> gensym)

resumingUnspecialized :: (AbstractHole value, Effectful (m value), Effects effects, Functor (m value effects), Member Trace effects) => m value (Resumable (Unspecialized value) ': effects) a -> m value effects a
resumingUnspecialized = runUnspecializedWith (\ err@(Unspecialized _) -> trace ("Unspecialized: " <> prettyShow err) $> hole)

resumingAddressError :: (AbstractHole value, Applicative (m address value effects), Effectful (m address value), Effects effects, Lower (Cell address value), Member Trace effects, Show address) => m address value (Resumable (AddressError address value) ': effects) a -> m address value effects a
resumingAddressError = runAddressErrorWith $ \ err -> trace ("AddressError: " <> prettyShow err) *> case err of
  Unallocated   _ -> pure lowerBound
  Uninitialized _ -> pure hole

resumingValueError :: (Applicative (m address (Value address body) effects), Effectful (m address (Value address body)), Effects effects, Member Trace effects, Show address) => m address (Value address body) (Resumable (ValueError address body) ': effects) a -> m address (Value address body) effects a
resumingValueError = runValueErrorWith (\ err -> trace ("ValueError: " <> prettyShow err) *> case err of
  CallError val     -> pure val
  StringError val   -> pure (pack (prettyShow val))
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

resumingEnvironmentError :: (Applicative (m (Hole (Maybe Name) address) value effects), Effectful (m (Hole (Maybe Name) address) value), Effects effects) => m (Hole (Maybe Name) address) value (Resumable (EnvironmentError (Hole (Maybe Name) address)) ': effects) a -> m (Hole (Maybe Name) address) value effects a
resumingEnvironmentError = runResumableWith (\ (FreeVariable name) -> pure (Partial (Just name)))

resumingTypeError :: ( Alternative (m address Type (State TypeMap ': effects))
                     , Effects effects
                     , Effectful (m address Type)
                     , Member Trace effects
                     )
                  => m address Type (Resumable TypeError ': State TypeMap ': effects) a
                  -> m address Type effects a
resumingTypeError = runTypesWith (\err -> trace ("TypeError: " <> prettyShow err) *> case err of
  UnificationError l r -> pure l <|> pure r
  InfiniteType _ r -> pure r)

prettyShow :: Show a => a -> String
prettyShow = hscolour TTY defaultColourPrefs False False "" False . ppShow
