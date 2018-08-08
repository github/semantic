{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators, TupleSections #-}
module Semantic.Graph
( runGraph
, runCallGraph
, runImportGraph
, runImportGraphToModules
, runImportGraphToModuleInfos
, GraphType(..)
, Graph
, Vertex
, ImportGraphEff(..)
, style
, parsePackage
, parsePythonPackage
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
import           Control.Abstract.PythonPackage as PythonPackage
import           Data.Abstract.Address
import           Data.Abstract.BaseError (BaseError(..))
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package as Package
import           Data.Abstract.Value.Abstract as Abstract
import           Data.Abstract.Value.Concrete as Concrete (Value, ValueError (..), runFunction, runValueErrorWith)
import           Data.Abstract.Value.Type as Type
import           Data.Coerce
import           Data.Graph
import           Data.Graph.Vertex (VertexDeclarationStrategy, VertexDeclarationWithStrategy)
import           Data.Project
import           Data.Record
import           Data.Term
import           Data.Text (pack, unpack)
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Parsing.Parser
import           Prologue hiding (MonadError (..), TypeError (..))
import           Semantic.Task as Task
import           Text.Show.Pretty (ppShow)
import System.FilePath.Posix ((</>), takeDirectory)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Language as Language

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
    runImportGraphToModuleInfos lang package
runGraph CallGraph includePackages project
  | SomeAnalysisParser parser lang <- someAnalysisParser (Proxy :: Proxy AnalysisClasses) (projectLanguage project) = do
    package <- parsePackage parser project
    modules <- topologicalSort <$> runImportGraphToModules lang package
    runCallGraph lang includePackages modules package

runCallGraph :: ( HasField fields Span
                , Show (Record fields)
                , Ord (Record fields)
                , (VertexDeclarationWithStrategy (VertexDeclarationStrategy syntax) syntax syntax)
                , Declarations1 syntax
                , Ord1 syntax
                , Functor syntax
                , Evaluatable syntax
                , term ~ Term syntax (Record fields)
                , FreeVariables term
                , Recursive term
                , HasPrelude lang
                , HasPostlude lang
                , Member Trace effs
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
      extractGraph (graph, _) = simplify graph
      runGraphAnalysis
        = runTermEvaluator @_ @(Hole (Maybe Name) (Located Monovariant)) @Abstract
        . graphing @_ @_ @(Maybe Name) @Monovariant
        . caching
        . runState (lowerBound @(Heap (Hole (Maybe Name) (Located Monovariant)) All Abstract))
        . runFresh 0
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . runReader (packageInfo package)
        . runReader (lowerBound @Span)
        . runReader (lowerBound @Vertex)
        . providingLiveSet
        . runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult (Hole (Maybe Name) (Located Monovariant)))))))
        . raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
  extractGraph <$> runEvaluator (runGraphAnalysis (evaluate lang analyzeModule analyzeTerm Abstract.runFunction modules))

runImportGraphToModuleInfos :: forall effs lang term.
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
               -> Eff effs (Graph Vertex)
runImportGraphToModuleInfos lang (package :: Package term) = runImportGraph lang package allModuleInfos
  where allModuleInfos info = maybe (vertex (unknownModuleVertex info)) (foldMap (vertex . moduleVertex . moduleInfo)) (ModuleTable.lookup (modulePath info) (packageModules package))

runImportGraphToModules :: forall effs lang term.
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
runImportGraphToModules lang (package :: Package term) = runImportGraph lang package resolveOrLowerBound
  where resolveOrLowerBound info = maybe lowerBound (foldMap vertex) (ModuleTable.lookup (modulePath info) (packageModules package))

runImportGraph :: forall effs lang term vertex.
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
               -> (ModuleInfo -> Graph vertex)
               -> Eff effs (Graph vertex)
runImportGraph lang (package :: Package term) f =
  let analyzeModule = graphingModuleInfo
      extractGraph (_, (graph, _)) = graph >>= f
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
        . runTermEvaluator @_ @_ @(Value (Hole (Maybe Name) Precise) (ImportGraphEff (Hole (Maybe Name) Precise) effs))
        . runReader (packageInfo package)
        . runReader lowerBound
  in extractGraph <$> runEvaluator (runImportGraphAnalysis (evaluate lang analyzeModule id (Concrete.runFunction coerce coerce) (ModuleTable.toPairs (packageModules package) >>= toList . snd)))

newtype ImportGraphEff address outerEffects a = ImportGraphEff
  { runImportGraphEff :: Eff (  Function address (Value address (ImportGraphEff address outerEffects))
                             ': Exc (LoopControl address)
                             ': Exc (Return address)
                             ': Env address
                             ': Deref address (Value address (ImportGraphEff address outerEffects))
                             ': Allocator address (Value address (ImportGraphEff address outerEffects))
                             ': Reader ModuleInfo
                             ': Reader Span
                             ': Reader PackageInfo
                             ': Modules address
                             ': Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))
                             ': State (Graph ModuleInfo)
                             ': Resumable (BaseError (ValueError address (ImportGraphEff address outerEffects)))
                             ': Resumable (BaseError (AddressError address (Value address (ImportGraphEff address outerEffects))))
                             ': Resumable (BaseError ResolutionError)
                             ': Resumable (BaseError EvalError)
                             ': Resumable (BaseError (EnvironmentError address))
                             ': Resumable (BaseError (UnspecializedError (Value address (ImportGraphEff address outerEffects))))
                             ': Resumable (BaseError (LoadError address))
                             ': Fresh
                             ': State (Heap address Latest (Value address (ImportGraphEff address outerEffects)))
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


-- | Parse a list of packages from a python project.
parsePythonPackage :: forall syntax fields effs term.
                   ( Declarations1 syntax
                   , Evaluatable syntax
                   , FreeVariables1 syntax
                   , Functor syntax
                   , term ~ Term syntax (Record fields)
                   , Member (Exc SomeException) effs
                   , Member Distribute effs
                   , Member Resolution effs
                   , Member Trace effs
                   , Member Task effs
                   , (Show (Record fields))
                   , Effects effs)
                   => Parser term       -- ^ A parser.
                   -> Maybe File        -- ^ Prelude (optional).
                   -> Project           -- ^ Project to parse into a package.
                   -> Eff effs (Package term, Strategy)
parsePythonPackage parser preludeFile project = do
  prelude <- traverse (parseModule project parser) preludeFile

  setupFile <- maybeM (error "no setup.py found in project") (find ((== (projectRootDir project </> "setup.py")) . filePath) (projectFiles project))
  setupModule <- parseModule project parser setupFile

  let runAnalysis = runEvaluator
        . runState PythonPackage.Unknown
        . runState lowerBound
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
        . runModules lowerBound
        . runTermEvaluator @_ @_ @(Value (Hole (Maybe Name) Precise) (ImportGraphEff (Hole (Maybe Name) Precise) (State Strategy ': effs)))
        . runReader (PackageInfo (name "setup") lowerBound)
        . runReader lowerBound
  (strat, _) <- runAnalysis $ evaluate (Proxy @'Language.Python) id id (Concrete.runFunction coerce coerce . runPythonPackaging) (maybeToList prelude <> [ setupModule ])
  case strat of
    PythonPackage.Unknown -> do
      modules <- parseModules parser project
      resMap <- Task.resolutionMap project
      pure (Package.fromModules (name (projectName project)) modules resMap, strat)
    PythonPackage.Packages dirs -> do
      filteredBlobs <- for dirs $ \dir -> do
        let packageDir = projectRootDir project </> unpack dir
        let paths = filter ((packageDir `isPrefixOf`) . filePath) (projectFiles project)
        traverse (readFile project) paths

      (, strat) <$> packageFromProject project filteredBlobs
    PythonPackage.FindPackages excludeDirs -> do
      trace "In Graph.FindPackages"
      let initFiles = filter (("__init__.py" `isSuffixOf`) . filePath) (projectFiles project)
      let packageDirs = filter (`notElem` ((projectRootDir project </>) . unpack <$> excludeDirs)) (takeDirectory . filePath <$> initFiles)
      filteredBlobs <- for packageDirs $ \dir -> do
        let paths = filter ((dir `isPrefixOf`) . filePath) (projectFiles project)
        traverse (readFile project) paths
      (, strat) <$> packageFromProject project filteredBlobs
    where
      packageFromProject project filteredBlobs = do
        let p = project { projectBlobs = catMaybes $ join filteredBlobs }
        modules <- parseModules parser p
        resMap <- Task.resolutionMap p
        pure (Package.fromModules (name $ projectName p) modules resMap)

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

resumingResolutionError :: ( Applicative (m effects)
                           , Effectful m
                           , Member Trace effects
                           , Effects effects
                           )
                         => m (Resumable (BaseError ResolutionError) ': effects) a
                         -> m effects a
resumingResolutionError = runResolutionErrorWith (\ baseError -> traceError "ResolutionError" baseError *> case baseErrorException baseError of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: ( Applicative (m address value effects)
                     , AbstractHole address
                     , Effectful (m address value)
                     , Effects effects
                     , Member Trace effects
                     )
                  => m address value (Resumable (BaseError (LoadError address)) ': effects) a
                  -> m address value effects a
resumingLoadError = runLoadErrorWith (\ baseError -> traceError "LoadError" baseError *> case baseErrorException baseError of
  ModuleNotFoundError _ -> pure (lowerBound, hole))

resumingEvalError :: ( Applicative (m effects)
                     , Effectful m
                     , Effects effects
                     , Member Fresh effects
                     , Member Trace effects
                     )
                  => m (Resumable (BaseError EvalError) ': effects) a
                  -> m effects a
resumingEvalError = runEvalErrorWith (\ baseError -> traceError "EvalError" baseError *> case baseErrorException baseError of
  DefaultExportError{}  -> pure ()
  ExportError{}         -> pure ()
  IntegerFormatError{}  -> pure 0
  FloatFormatError{}    -> pure 0
  RationalFormatError{} -> pure 0
  NoNameError           -> gensym)

resumingUnspecialized :: ( Applicative (m value effects)
                         , AbstractHole value
                         , Effectful (m value)
                         , Effects effects
                         , Member Trace effects)
                      => m value (Resumable (BaseError (UnspecializedError value)) ': effects) a
                      -> m value effects a
resumingUnspecialized = runUnspecializedWith (\ baseError -> traceError "UnspecializedError" baseError *> case baseErrorException baseError of
  UnspecializedError _ -> pure hole)

resumingAddressError :: ( AbstractHole value
                        , Applicative (m address value effects)
                        , Effectful (m address value)
                        , Effects effects
                        , Lower (Cell address value)
                        , Member Trace effects
                        , Show address
                        )
                     => m address value (Resumable (BaseError (AddressError address value)) ': effects) a
                     -> m address value effects a
resumingAddressError = runAddressErrorWith $ \ baseError -> traceError "AddressError" baseError *> case baseErrorException baseError of
  UnallocatedAddress   _ -> pure lowerBound
  UninitializedAddress _ -> pure hole

resumingValueError :: ( Applicative (m address (Value address body) effects)
                      , Effectful (m address (Value address body))
                      , Effects effects
                      , Member Trace effects
                      , Show address
                      )
                   => m address (Value address body) (Resumable (BaseError (ValueError address body)) ': effects) a
                   -> m address (Value address body) effects a
resumingValueError = runValueErrorWith (\ baseError -> traceError "ValueError" baseError *> case baseErrorException baseError of
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
  ArrayError{}   -> pure lowerBound
  ArithmeticError{} -> pure hole)

resumingEnvironmentError :: ( Monad (m (Hole (Maybe Name) address) value effects)
                            , Effectful (m (Hole (Maybe Name) address) value)
                            , Effects effects
                            , Member Trace effects
                            )
                         => m (Hole (Maybe Name) address) value (Resumable (BaseError (EnvironmentError (Hole (Maybe Name) address))) ': effects) a
                         -> m (Hole (Maybe Name) address) value effects a
resumingEnvironmentError = runResumableWith (\ baseError -> traceError "EnvironmentError" baseError >> (\ (FreeVariable name) -> pure (Partial (Just name))) (baseErrorException baseError))

resumingTypeError :: ( Alternative (m address Type (State TypeMap ': effects))
                     , Effects effects
                     , Effectful (m address Type)
                     , Member Trace effects
                     )
                  => m address Type (Resumable (BaseError TypeError) ': State TypeMap ': effects) a
                  -> m address Type effects a
resumingTypeError = runTypesWith (\ baseError -> traceError "TypeError" baseError *> case baseErrorException baseError of
  UnificationError l r -> pure l <|> pure r
  InfiniteType _ r -> pure r)

prettyShow :: Show a => a -> String
prettyShow = hscolour TTY defaultColourPrefs False False "" False . ppShow

traceError :: (Member Trace effects, Effectful m, Show (exc resume)) => String -> BaseError exc resume -> m effects ()
traceError prefix baseError = trace $ prefix <> ": " <> prettyShow baseError
