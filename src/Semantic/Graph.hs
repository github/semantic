{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators #-}
module Semantic.Graph
( runGraph
, runCallGraph
, runImportGraph
, runImportGraphToModules
, runImportGraphToModuleInfos
, GraphType(..)
, Graph
, ControlFlowVertex
, style
, runHeap
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

import           Analysis.Abstract.Caching.FlowInsensitive
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Graph as Graph
import           Control.Abstract
import           Control.Abstract.PythonPackage as PythonPackage
import           Data.Abstract.Address.Hole as Hole
import           Data.Abstract.Address.Located as Located
import           Data.Abstract.Address.Monovariant as Monovariant
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.BaseError (BaseError (..))
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package as Package
import           Data.Abstract.Value.Abstract as Abstract
import           Data.Abstract.Value.Concrete as Concrete
    (Value, ValueError (..), runValueErrorWith)
import           Data.Abstract.Value.Type as Type
import           Data.Blob
import           Data.Graph
import           Data.Graph.ControlFlowVertex (VertexDeclarationStrategy, VertexDeclarationWithStrategy)
import           Data.Language as Language
import           Data.List (isPrefixOf, isSuffixOf)
import           Data.Project
import           Data.Location
import           Data.Term
import           Data.Text (pack, unpack)
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Parsing.Parser
import           Prologue hiding (MonadError (..), TypeError (..))
import           Semantic.Task as Task
import           System.FilePath.Posix (takeDirectory, (</>))
import           Text.Show.Pretty (ppShow)

data GraphType = ImportGraph | CallGraph

type AnalysisClasses = '[ Declarations1, Eq1, Evaluatable, FreeVariables1, Foldable, Functor, Ord1, Show1 ]

runGraph :: (Member Distribute sig, Member (Error SomeException) sig, Member Resolution sig, Member Task sig, Member Trace sig, Carrier sig m, Monad m)
         => GraphType
         -> Bool
         -> Project
         -> m (Graph ControlFlowVertex)
runGraph ImportGraph _ project
  | SomeAnalysisParser parser (lang' :: Proxy lang) <- someAnalysisParser (Proxy :: Proxy AnalysisClasses) (projectLanguage project) = do
    let parse = if projectLanguage project == Language.Python then parsePythonPackage parser else fmap (fmap snd) . parsePackage parser
    package <- parse project
    runImportGraphToModuleInfos lang' package
runGraph CallGraph includePackages project
  | SomeAnalysisParser parser lang <- someAnalysisParser (Proxy :: Proxy AnalysisClasses) (projectLanguage project) = do
    let parse = if projectLanguage project == Language.Python then parsePythonPackage parser else fmap (fmap snd) . parsePackage parser
    package <- parse project
    modules <- topologicalSort <$> runImportGraphToModules lang package
    runCallGraph lang includePackages modules package

runCallGraph :: ( VertexDeclarationWithStrategy (VertexDeclarationStrategy syntax) syntax syntax
                , Declarations1 syntax
                , Ord1 syntax
                , Functor syntax
                , Evaluatable syntax
                , term ~ Term syntax Location
                , FreeVariables term
                , Recursive term
                , HasPrelude lang
                , HasPostlude lang
                , Member Trace sig
                , Carrier sig m
                )
             => Proxy lang
             -> Bool
             -> [Module term]
             -> Package term
             -> m (Graph ControlFlowVertex)
runCallGraph lang includePackages modules package = do
  let analyzeTerm = withTermSpans . graphingTerms . cachingTerms
      analyzeModule = (if includePackages then graphingPackages else id) . convergingModules . graphingModules
      extractGraph (graph, _) = simplify graph
      runGraphAnalysis
        = graphing @_ @_ @(Maybe Name) @Monovariant
        . runHeap
        . caching
        . runFresh
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . runReader (packageInfo package)
        . runReader (lowerBound @Span)
        . runState (lowerBound @Span)
        . runReader (lowerBound @ControlFlowVertex)
        . providingLiveSet
        . runModuleTable
        . runModules (ModuleTable.modulePaths (packageModules package))
  extractGraph <$> runEvaluator (runGraphAnalysis (evaluate lang analyzeModule analyzeTerm modules))

runModuleTable :: Carrier sig m
               => Evaluator term address value (ReaderC (ModuleTable (NonEmpty (Module (ModuleResult address))))
                 (Evaluator term address value m)) a
               -> Evaluator term address value m a
runModuleTable = runReader lowerBound . runEvaluator

runImportGraphToModuleInfos :: ( Declarations term
                               , Evaluatable (Base term)
                               , FreeVariables term
                               , HasPrelude lang
                               , HasPostlude lang
                               , Member Trace sig
                               , Recursive term
                               , Carrier sig m
                               , Show term
                               )
                            => Proxy lang
                            -> Package term
                            -> m (Graph ControlFlowVertex)
runImportGraphToModuleInfos lang (package :: Package term) = runImportGraph lang package allModuleInfos
  where allModuleInfos info = maybe (vertex (unknownModuleVertex info)) (foldMap (vertex . moduleVertex . moduleInfo)) (ModuleTable.lookup (modulePath info) (packageModules package))

runImportGraphToModules :: ( Declarations term
                           , Evaluatable (Base term)
                           , FreeVariables term
                           , HasPrelude lang
                           , HasPostlude lang
                           , Member Trace sig
                           , Recursive term
                           , Carrier sig m
                           , Show term
                           )
                        => Proxy lang
                        -> Package term
                        -> m (Graph (Module term))
runImportGraphToModules lang (package :: Package term) = runImportGraph lang package resolveOrLowerBound
  where resolveOrLowerBound info = maybe lowerBound (foldMap vertex) (ModuleTable.lookup (modulePath info) (packageModules package))

runImportGraph :: ( Declarations term
                  , Evaluatable (Base term)
                  , FreeVariables term
                  , HasPrelude lang
                  , HasPostlude lang
                  , Member Trace sig
                  , Recursive term
                  , Carrier sig m
                  , Show term
                  )
               => Proxy lang
               -> Package term
               -> (ModuleInfo -> Graph vertex)
               -> m (Graph vertex)
runImportGraph lang (package :: Package term) f =
  let analyzeModule = graphingModuleInfo
      extractGraph (graph, _) = graph >>= f
      runImportGraphAnalysis
        = runState lowerBound
        . runHeap
        . runFresh
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . resumingValueError
        . runModuleTable
        . runModules (ModuleTable.modulePaths (packageModules package))
        . runReader (packageInfo package)
        . runState (lowerBound @Span)
        . runReader (lowerBound @Span)
  in extractGraph <$> runEvaluator @_ @_ @(Value _ (Hole (Maybe Name) Precise)) (runImportGraphAnalysis (evaluate lang analyzeModule id (ModuleTable.toPairs (packageModules package) >>= toList . snd)))


runHeap :: (Carrier sig m, Effect sig) => Evaluator term address value (StateC (Heap address value) (Evaluator term address value m)) a -> Evaluator term address value m (Heap address value, a)
runHeap = runState lowerBound

-- | Parse a list of files into a 'Package'.
parsePackage :: (Member Distribute sig, Member (Error SomeException) sig, Member Resolution sig, Member Task sig, Member Trace sig, Carrier sig m, Monad m)
             => Parser term -- ^ A parser.
             -> Project     -- ^ Project to parse into a package.
             -> m (Package (Blob, term))
parsePackage parser project = do
  p <- parseModules parser project
  resMap <- Task.resolutionMap project
  let pkg = Package.fromModules n p resMap
  pkg <$ trace ("project: " <> prettyShow (() <$ pkg))

  where
    n = name (projectName project)

-- | Parse all files in a project into 'Module's.
parseModules :: (Member Distribute sig, Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m) => Parser term -> Project -> m [Module (Blob, term)]
parseModules parser p@Project{..} = distributeFor (projectFiles p) (parseModule p parser)


-- | Parse a list of packages from a python project.
parsePythonPackage :: forall syntax sig m term.
                   ( Declarations1 syntax
                   , Evaluatable syntax
                   , FreeVariables1 syntax
                   , Functor syntax
                   , term ~ Term syntax Location
                   , Member (Error SomeException) sig
                   , Member Distribute sig
                   , Member Resolution sig
                   , Member Trace sig
                   , Member Task sig
                   , Carrier sig m
                   , Monad m
                   )
                   => Parser term      -- ^ A parser.
                   -> Project          -- ^ Project to parse into a package.
                   -> m (Package term)
parsePythonPackage parser project = do
  let runAnalysis = runEvaluator @_ @_ @(Value term (Hole (Maybe Name) Precise))
        . runState PythonPackage.Unknown
        . runState (lowerBound @(Heap (Hole (Maybe Name) Precise) (Value term (Hole (Maybe Name) Precise))))
        . runFresh
        . resumingLoadError
        . resumingUnspecialized
        . resumingEnvironmentError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . resumingValueError
        . runModuleTable
        . runModules lowerBound
        . runReader (PackageInfo (name "setup") lowerBound)
        . runState (lowerBound @Span)
        . runReader (lowerBound @Span)

  strat <- case find ((== (projectRootDir project </> "setup.py")) . filePath) (projectFiles project) of
    Just setupFile -> do
      setupModule <- fmap snd <$> parseModule project parser setupFile
      fst <$> runAnalysis (evaluate (Proxy @'Language.Python) id id runPythonPackaging [ setupModule ])
    Nothing -> pure PythonPackage.Unknown
  case strat of
    PythonPackage.Unknown -> do
      modules <- fmap (fmap snd) <$> parseModules parser project
      resMap <- Task.resolutionMap project
      pure (Package.fromModules (name (projectName project)) modules resMap)
    PythonPackage.Packages dirs -> do
      filteredBlobs <- for dirs $ \dir -> do
        let packageDir = projectRootDir project </> unpack dir
        let paths = filter ((packageDir `isPrefixOf`) . filePath) (projectFiles project)
        traverse (readFile project) paths
      packageFromProject project filteredBlobs
    PythonPackage.FindPackages excludeDirs -> do
      trace "In Graph.FindPackages"
      let initFiles = filter (("__init__.py" `isSuffixOf`) . filePath) (projectFiles project)
      let packageDirs = filter (`notElem` ((projectRootDir project </>) . unpack <$> excludeDirs)) (takeDirectory . filePath <$> initFiles)
      filteredBlobs <- for packageDirs $ \dir -> do
        let paths = filter ((dir `isPrefixOf`) . filePath) (projectFiles project)
        traverse (readFile project) paths
      packageFromProject project filteredBlobs
    where
      packageFromProject project filteredBlobs = do
        let p = project { projectBlobs = catMaybes $ join filteredBlobs }
        modules <- fmap (fmap snd) <$> parseModules parser p
        resMap <- Task.resolutionMap p
        pure (Package.fromModules (name $ projectName p) modules resMap)

parseModule :: (Member (Error SomeException) sig, Member Task sig, Carrier sig m, Monad m)
            => Project
            -> Parser term
            -> File
            -> m (Module (Blob, term))
parseModule proj parser file = do
  mBlob <- readFile proj file
  case mBlob of
    Just blob -> moduleForBlob (Just (projectRootDir proj)) blob . (,) blob <$> parse parser blob
    Nothing   -> throwError (SomeException (FileNotFound (filePath file)))

withTermSpans :: ( Member (Reader Span) sig
                 , Member (State Span) sig -- last evaluated child's span
                 , Recursive term
                 , Carrier sig m
                 , Base term ~ TermF syntax Location
                 )
              => Open (Open (term -> Evaluator term address value m a))
withTermSpans recur0 recur term = let
  span = locationSpan (termFAnnotation (project term))
  updatedSpanAlg = withCurrentSpan span (recur0 recur term)
  in modifyChildSpan span updatedSpanAlg

resumingResolutionError :: ( Member Trace sig
                           , Carrier sig m
                           )
                         => Evaluator term address value (ResumableWithC (BaseError ResolutionError)
                           (Evaluator term address value m)) a
                         -> Evaluator term address value m a
resumingResolutionError = runResolutionErrorWith (\ baseError -> traceError "ResolutionError" baseError *> case baseErrorException baseError of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: ( AbstractHole address
                     , Carrier sig m
                     , Member Trace sig
                     , Ord address
                     )
                  => Evaluator term address value (ResumableWithC (BaseError (LoadError address))
                    (Evaluator term address value m)) a
                  -> Evaluator term address value m a
resumingLoadError = runLoadErrorWith (\ baseError -> traceError "LoadError" baseError *> case baseErrorException baseError of
  ModuleNotFoundError _ -> pure (lowerBound, (lowerBound, hole)))

resumingEvalError :: ( Carrier sig m
                     , Member Fresh sig
                     , Member Trace sig
                     )
                  => Evaluator term address value (ResumableWithC (BaseError EvalError)
                    (Evaluator term address value m)) a
                  -> Evaluator term address value m a
resumingEvalError = runEvalErrorWith (\ baseError -> traceError "EvalError" baseError *> case baseErrorException baseError of
  DefaultExportError{}  -> pure ()
  ExportError{}         -> pure ()
  IntegerFormatError{}  -> pure 0
  FloatFormatError{}    -> pure 0
  RationalFormatError{} -> pure 0
  NoNameError           -> gensym)

resumingUnspecialized :: ( AbstractHole value
                         , Carrier sig m
                         , Member Trace sig
                         )
                      => Evaluator term address value (ResumableWithC (BaseError (UnspecializedError value))
                        (Evaluator term address value m)) a
                      -> Evaluator term address value m a
resumingUnspecialized = runUnspecializedWith (\ baseError -> traceError "UnspecializedError" baseError *> case baseErrorException baseError of
  UnspecializedError _ -> pure hole)

resumingAddressError :: ( AbstractHole value
                        , Carrier sig m
                        , Member Trace sig
                        , Show address
                        )
                     => Evaluator term address value (ResumableWithC (BaseError (AddressError address value))
                       (Evaluator term address value m)) a
                     -> Evaluator term address value m a
resumingAddressError = runAddressErrorWith $ \ baseError -> traceError "AddressError" baseError *> case baseErrorException baseError of
  UnallocatedAddress   _ -> pure lowerBound
  UninitializedAddress _ -> pure hole

resumingValueError :: ( Carrier sig m
                      , Member Trace sig
                      , Show address
                      , Show term
                      )
                   => Evaluator term address (Value term address) (ResumableWithC (BaseError (ValueError term address))
                     (Evaluator term address (Value term address) m)) a
                   -> Evaluator term address (Value term address) m a
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
  ArrayError{}      -> pure lowerBound
  ArithmeticError{} -> pure hole)

resumingEnvironmentError :: ( Carrier sig m
                            , Member Trace sig
                            )
                         => Evaluator term (Hole (Maybe Name) address) value (ResumableWithC (BaseError (EnvironmentError (Hole (Maybe Name) address)))
                           (Evaluator term (Hole (Maybe Name) address) value m)) a
                         -> Evaluator term (Hole (Maybe Name) address) value m a
resumingEnvironmentError = runResumableWith (\ baseError -> traceError "EnvironmentError" baseError >> (\ (FreeVariable name) -> pure (Partial (Just name))) (baseErrorException baseError))

resumingTypeError :: ( Carrier sig m
                     , Member NonDet sig
                     , Member Trace sig
                     )
                  => Evaluator term address Type (ResumableWithC (BaseError TypeError)
                    (Evaluator term address Type (StateC TypeMap
                    (Evaluator term address Type m)))) a
                  -> Evaluator term address Type m a
resumingTypeError = runTypesWith (\ baseError -> traceError "TypeError" baseError *> case baseErrorException baseError of
  UnificationError l r -> pure l <|> pure r
  InfiniteType _ r     -> pure r)

prettyShow :: Show a => a -> String
prettyShow = hscolour TTY defaultColourPrefs False False "" False . ppShow

traceError :: (Member Trace sig, Show (exc resume), Carrier sig m) => String -> BaseError exc resume -> Evaluator term address value m ()
traceError prefix baseError = trace $ prefix <> ": " <> prettyShow baseError
