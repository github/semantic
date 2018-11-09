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
, runModuleTable
, parsePackage
, parsePythonPackage
, withTermSpans
, resumingResolutionError
, resumingLoadError
, resumingEvalError
, resumingUnspecialized
, resumingAddressError
, resumingValueError
-- , resumingEnvironmentError -- TODO: Fix me. Replace with resumingScopeGraphError?
, resumingTypeError
) where


import Prelude hiding (readFile)

import           Analysis.Abstract.Caching.FlowInsensitive
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Graph as Graph
import           Control.Abstract
import Control.Abstract.Heap as Heap
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
import           Data.File
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
import           Prologue hiding (TypeError (..))
import           Semantic.Analysis
import           Semantic.Task as Task
import           System.FilePath.Posix (takeDirectory, (</>))
import           Text.Show.Pretty (ppShow)

data GraphType = ImportGraph | CallGraph

type AnalysisClasses = '[ Declarations1, Eq1, Evaluatable, FreeVariables1, Foldable, Functor, Ord1, Show1 ]

runGraph :: ( Member Distribute sig
            , Member (Error SomeException) sig
            , Member Resolution sig
            , Member Task sig
            , Member Trace sig
            , Carrier sig m
            , Effect sig
            )
         => GraphType
         -> Bool
         -> Project
         -> Eff m (Graph ControlFlowVertex)
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
                , FreeVariables1 syntax
                , HasPrelude lang
                , Member Trace sig
                , Carrier sig m
                , Effect sig
                )
             => Proxy lang
             -> Bool
             -> [Module term]
             -> Package term
             -> Eff m (Graph ControlFlowVertex)
runCallGraph lang includePackages modules package
  = fmap (simplify . fst)
  . runEvaluator
  . graphing @_ @_ @_ @(Hole (Maybe Name) (Located Monovariant)) @Abstract
  . runHeap
  . caching
  . raiseHandler (runState (lowerBound @(ScopeGraph (Hole (Maybe Name) (Located Monovariant)))))
  . raiseHandler (runState (lowerBound @(Heap (Hole (Maybe Name) (Located Monovariant)) (Hole (Maybe Name) (Located Monovariant)) Abstract)))
  . raiseHandler runFresh
  . resumingLoadError
  . resumingUnspecialized
  . resumingScopeError
  . resumingHeapError
  . resumingEvalError
  . resumingResolutionError
  . resumingAddressError
  . raiseHandler (runReader (packageInfo package))
  . raiseHandler (runReader (lowerBound @Span))
  . raiseHandler (runState (lowerBound @Span))
  . raiseHandler (runReader (lowerBound @ControlFlowVertex))
  . providingLiveSet
  . runModuleTable
  . runModules (ModuleTable.modulePaths (packageModules package))
  $ evaluate lang perModule perTerm modules
  where perTerm = evalTerm (withTermSpans . graphingTerms . cachingTerms)
        perModule = (if includePackages then graphingPackages else id) . convergingModules . graphingModules


runModuleTable :: Carrier sig m
               => Evaluator term address value (ReaderC (ModuleTable (NonEmpty (Module (ModuleResult address value)))) (Eff m)) a
               -> Evaluator term address value m a
runModuleTable = raiseHandler $ runReader lowerBound

runImportGraphToModuleInfos :: ( Declarations term
                               , Evaluatable (Base term)
                               , FreeVariables term
                               , HasPrelude lang
                               , Member Trace sig
                               , Recursive term
                               , Carrier sig m
                               , Show term
                               , Effect sig
                               )
                            => Proxy lang
                            -> Package term
                            -> Eff m (Graph ControlFlowVertex)
runImportGraphToModuleInfos lang (package :: Package term) = runImportGraph lang package allModuleInfos
  where allModuleInfos info = maybe (vertex (unknownModuleVertex info)) (foldMap (vertex . moduleVertex . moduleInfo)) (ModuleTable.lookup (modulePath info) (packageModules package))

runImportGraphToModules :: ( Declarations term
                           , Evaluatable (Base term)
                           , FreeVariables term
                           , HasPrelude lang
                           , Member Trace sig
                           , Recursive term
                           , Carrier sig m
                           , Show term
                           , Effect sig
                           )
                        => Proxy lang
                        -> Package term
                        -> Eff m (Graph (Module term))
runImportGraphToModules lang (package :: Package term) = runImportGraph lang package resolveOrLowerBound
  where resolveOrLowerBound info = maybe lowerBound (foldMap vertex) (ModuleTable.lookup (modulePath info) (packageModules package))

runImportGraph :: ( Declarations term
                  , Evaluatable (Base term)
                  , FreeVariables term
                  , HasPrelude lang
                  , Member Trace sig
                  , Recursive term
                  , Carrier sig m
                  , Show term
                  , Effect sig
                  )
               => Proxy lang
               -> Package term
               -> (ModuleInfo -> Graph vertex)
               -> Eff m (Graph vertex)
runImportGraph lang (package :: Package term) f
  = fmap (fst >=> f)
  . runEvaluator @_ @_ @(Value _ (Hole (Maybe Name) Precise))
  . raiseHandler (runState lowerBound)
  . runHeap
  . raiseHandler runFresh
  . resumingLoadError
  . resumingUnspecialized
  . resumingScopeError
  . resumingHeapError
  . resumingEvalError
  . resumingResolutionError
  . resumingAddressError
  . resumingValueError
  . runModuleTable
  . runModules (ModuleTable.modulePaths (packageModules package))
  . raiseHandler (runReader (packageInfo package))
  . raiseHandler (runState (lowerBound @Span))
  . raiseHandler (runReader (lowerBound @Span))
  $ evaluate lang graphingModuleInfo (evalTerm id) (ModuleTable.toPairs (packageModules package) >>= toList . snd)

runHeap :: (Carrier sig m, Effect sig)
        => Evaluator term address value (StateC (Heap address address value) (Eff m)) a
        -> Evaluator term address value m (Heap address address value, a)
runHeap = raiseHandler (runState lowerBound)

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
    n = Data.Abstract.Evaluatable.name (projectName project) -- TODO: Confirm this is the right `name`.

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
                   , Effect sig
                   )
                   => Parser term      -- ^ A parser.
                   -> Project          -- ^ Project to parse into a package.
                   -> Eff m (Package term)
parsePythonPackage parser project = do
  let runAnalysis = runEvaluator @_ @_ @(Value term (Hole (Maybe Name) Precise))
        . raiseHandler (runState PythonPackage.Unknown)
        . raiseHandler (runState (lowerBound @(Heap (Hole (Maybe Name) Precise) (Hole (Maybe Name) Precise) (Value term (Hole (Maybe Name) Precise)))))
        . raiseHandler runFresh
        . resumingLoadError
        . resumingUnspecialized
        -- . resumingEnvironmentError -- TODO: Fix me. Replace with `resumineScopeGraphError`?
        . resumingScopeError
        . resumingHeapError
        . resumingEvalError
        . resumingResolutionError
        . resumingAddressError
        . resumingValueError
        . runModuleTable
        . runModules lowerBound
        . raiseHandler (runReader (PackageInfo (Data.Abstract.Evaluatable.name "setup") lowerBound))
        . raiseHandler (runState (lowerBound @Span))
        . raiseHandler (runReader (lowerBound @Span))

  strat <- case find ((== (projectRootDir project </> "setup.py")) . filePath) (projectFiles project) of
    Just setupFile -> do
      setupModule <- fmap snd <$> parseModule project parser setupFile
      fst <$> runAnalysis (evaluate (Proxy @'Language.Python) id (runPythonPackaging . evalTerm id) [ setupModule ])
    Nothing -> pure PythonPackage.Unknown
  case strat of
    PythonPackage.Unknown -> do
      modules <- fmap (fmap snd) <$> parseModules parser project
      resMap <- Task.resolutionMap project
      pure (Package.fromModules (Data.Abstract.Evaluatable.name (projectName project)) modules resMap) -- TODO: Confirm this is the right `name`.
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
        pure (Package.fromModules (Data.Abstract.Evaluatable.name $ projectName p) modules resMap) -- TODO: Confirm this is the right `name`.

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
                         => Evaluator term address value (ResumableWithC (BaseError ResolutionError) (Eff
                                                         m)) a
                         -> Evaluator term address value m a
resumingResolutionError = runResolutionErrorWith (\ baseError -> traceError "ResolutionError" baseError *> case baseErrorException baseError of
  NotFoundError nameToResolve _ _ -> pure  nameToResolve
  GoImportError pathToResolve     -> pure [pathToResolve])

resumingLoadError :: ( Carrier sig m
                     , Member Trace sig
                     , Ord address
                     )
                  => Evaluator term address value (ResumableWithC (BaseError (LoadError address value)) (Eff m)) a
                  -> Evaluator term address value m a
resumingLoadError= runLoadErrorWith (\ baseError -> traceError "LoadError" baseError *> case baseErrorException baseError of
  ModuleNotFoundError _ -> pure (lowerBound, undefined))

resumingEvalError :: ( Carrier sig m
                     , Member Fresh sig
                     , Member Trace sig
                     )
                  => Evaluator term address value (ResumableWithC (BaseError EvalError) (Eff
                                                  m)) a
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
                      => Evaluator term address value (ResumableWithC (BaseError (UnspecializedError value)) (Eff
                                                      m)) a
                      -> Evaluator term address value m a
resumingUnspecialized = runUnspecializedWith (\ baseError -> traceError "UnspecializedError" baseError *> case baseErrorException baseError of
  UnspecializedError _ -> pure hole)

resumingAddressError :: ( AbstractHole value
                        , Carrier sig m
                        , Member Trace sig
                        , Show address
                        )
                     => Evaluator term address value (ResumableWithC (BaseError (AddressError address value)) (Eff
                                                     m)) a
                     -> Evaluator term address value m a
resumingAddressError = runAddressErrorWith $ \ baseError -> traceError "AddressError" baseError *> case baseErrorException baseError of
  UnallocatedAddress   _ -> pure lowerBound
  UninitializedAddress _ -> pure hole

resumingValueError :: ( Carrier sig m
                      , Member Trace sig
                      , Show address
                      , Show term
                      )
                   => Evaluator term address (Value term address) (ResumableWithC (BaseError (ValueError term address)) (Eff
                                                                  m)) a
                   -> Evaluator term address (Value term address) m a
resumingValueError = runValueErrorWith (\ baseError -> traceError "ValueError" baseError *> case baseErrorException baseError of
  CallError val     -> rvalBox val
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

resumingHeapError :: ( Carrier sig m
                     , AbstractHole address
                     , Member Trace sig
                     , Show address
                     , Ord address
                     , Member Fresh sig
                     , Member (Resumable (BaseError (ScopeError address))) sig
                     , Effect sig
                     )
                  => Evaluator term address value (ResumableWithC (BaseError (HeapError address)) (Eff m)) a
                  -> Evaluator term address value m a
resumingHeapError = runHeapErrorWith (\ baseError -> traceError "ScopeError" baseError *> case baseErrorException baseError of
    CurrentFrameError -> pure hole)

resumingScopeError :: ( Carrier sig m
                     , Member Trace sig
                     , AbstractHole (Address address)
                     , AbstractHole (Scope address)
                     , AbstractHole (Path address)
                     , AbstractHole address
                     , Show address
                     , Ord address
                     )
                    => Evaluator term address value (ResumableWithC (BaseError (ScopeError address)) (Eff m)) a
                    -> Evaluator term address value m a
resumingScopeError = runScopeErrorWith (\ baseError -> traceError "ScopeError" baseError *> case baseErrorException baseError of
  ScopeError decl span -> pure hole
  LookupScopeError -> pure hole
  LookupPathError decl -> pure hole
  CurrentScopeError -> pure hole)

resumingTypeError :: ( Carrier sig m
                     , Member NonDet sig
                     , Member Trace sig
                     , Effect sig
                     )
                  => Evaluator term address Type (ResumableWithC (BaseError TypeError) (Eff
                                                 (StateC TypeMap (Eff
                                                 m)))) a
                  -> Evaluator term address Type m a
resumingTypeError = runTypesWith (\ baseError -> traceError "TypeError" baseError *> case baseErrorException baseError of
  UnificationError l r -> pure l <|> pure r
  InfiniteType _ r     -> pure r)

prettyShow :: Show a => a -> String
prettyShow = hscolour TTY defaultColourPrefs False False "" False . ppShow

traceError :: (Member Trace sig, Show (exc resume), Carrier sig m) => String -> BaseError exc resume -> Evaluator term address value m ()
traceError prefix baseError = trace $ prefix <> ": " <> prettyShow baseError
