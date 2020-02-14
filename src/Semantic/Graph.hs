{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Semantic.Graph
( analysisParsers
, AnalyzeTerm
, runGraph
, runCallGraph
, runImportGraph
, runImportGraphToModules
, runImportGraphToModuleInfos
, GraphType(..)
, Graph
, ControlFlowVertex
, style
, runHeap
, runScopeGraph
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
, resumingHeapError
, resumingScopeError
, resumingTypeError
) where


import Prelude hiding (readFile)

import           Analysis.Abstract.Caching.FlowInsensitive
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Graph as Graph
import           Analysis.File
import           Analysis.Project
import           Control.Abstract hiding (String)
import           Control.Abstract.PythonPackage as PythonPackage
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.Resumable.Resume
import           Control.Carrier.State.Strict
import           Control.Effect.Parse
import           Control.Lens.Getter
import           Control.Monad
import           Data.Abstract.AccessControls.Instances ()
import           Data.Abstract.Address.Hole as Hole
import           Data.Abstract.Address.Monovariant as Monovariant
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.Evaluatable
import           Data.Abstract.Heap
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package as Package
import           Data.Abstract.Value.Abstract as Abstract
import           Data.Abstract.Value.Concrete as Concrete (Value, ValueError (..), runValueErrorWith)
import           Data.Abstract.Value.Type as Type
import           Data.Blob
import           Data.Functor.Foldable
import           Data.Graph.Algebraic
import           Data.Graph.ControlFlowVertex (VertexDeclaration)
import           Data.Language as Language
import           Data.List (find, isPrefixOf)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Text (pack, unpack)
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Parsing.Parser
import           Semantic.Analysis
import           Semantic.Task as Task
import           Source.Loc as Loc
import           Source.Span
import           System.FilePath.Posix (takeDirectory, (</>))
import qualified System.Path as Path
import           Text.Show.Pretty (ppShow)

data GraphType = ImportGraph | CallGraph

-- | Constraints required to analyze a term.
class
  ( AccessControls (term Loc)
  , Declarations (term Loc)
  , Evaluatable (Base (term Loc))
  , FreeVariables (term Loc)
  , HasSpan (term Loc)
  , Ord (term Loc)
  , Recursive (term Loc)
  , Show (term Loc)
  , VertexDeclaration term
  ) => AnalyzeTerm term

instance
  ( AccessControls (term Loc)
  , Declarations (term Loc)
  , Evaluatable (Base (term Loc))
  , FreeVariables (term Loc)
  , HasSpan (term Loc)
  , Ord (term Loc)
  , Recursive (term Loc)
  , Show (term Loc)
  , VertexDeclaration term
  ) => AnalyzeTerm term

analysisParsers :: Map Language (SomeParser AnalyzeTerm Loc)
analysisParsers = Map.fromList
  [ goParserALaCarte
  , javascriptParserALaCarte
  , pythonParserALaCarte
  , rubyParserALaCarte
  , typescriptParserALaCarte
  , tsxParserALaCarte
  ]

runGraph :: ( Has Distribute sig m
            , Has Parse sig m
            , Has Resolution sig m
            , Has Trace sig m
            , Effect sig
            )
         => GraphType
         -> Bool
         -> Project
         -> m (Graph ControlFlowVertex)
runGraph type' includePackages project
  | Just (SomeParser parser) <- parserForLanguage analysisParsers (projectLanguage project)
  , SomeLanguage (lang :: Proxy lang) <- reifyLanguage (projectLanguage project) = do
    package <- if projectLanguage project == Language.Python then
        parsePythonPackage parser project
      else
        fmap snd <$> parsePackage parser project
    case type' of
      ImportGraph -> runImportGraphToModuleInfos lang package
      CallGraph -> do
        modules <- topologicalSort <$> runImportGraphToModules lang package
        runCallGraph lang includePackages modules package
  | otherwise = error $ "Analysis not supported for: " <> show (projectLanguage project)

data SomeLanguage where
  SomeLanguage :: HasPrelude lang => Proxy lang -> SomeLanguage

reifyLanguage :: Language -> SomeLanguage
reifyLanguage = \case
  Go         -> SomeLanguage (Proxy @'Go)
  JavaScript -> SomeLanguage (Proxy @'JavaScript)
  PHP        -> SomeLanguage (Proxy @'PHP)
  Python     -> SomeLanguage (Proxy @'Python)
  Ruby       -> SomeLanguage (Proxy @'Ruby)
  TypeScript -> SomeLanguage (Proxy @'TypeScript)
  TSX        -> SomeLanguage (Proxy @'TSX)
  l          -> error $ "HasPrelude not supported for: " <> show l

runCallGraph :: ( AnalyzeTerm term
                , HasPrelude lang
                , Has Trace sig m
                , Effect sig
                )
             => Proxy lang
             -> Bool
             -> [Module (term Loc)]
             -> Package (term Loc)
             -> m (Graph ControlFlowVertex)
runCallGraph lang includePackages modules package
  = fmap (simplify . fst)
  . runEvaluator
  . graphing @_ @_ @_ @(Hole (Maybe Name) Monovariant) @Abstract
  . runHeap
  . runScopeGraph
  . caching
  . raiseHandler (runFresh 0)
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
  $ evaluate lang perModule modules
  where perTerm = evalTerm (withTermSpans (^. span_) . graphingTerms . cachingTerms)
        perModule = (if includePackages then graphingPackages else id) . convergingModules . graphingModules $ runDomainEffects perTerm


runModuleTable :: Evaluator term address value (ReaderC (ModuleTable (Module (ModuleResult address value))) m) a
               -> Evaluator term address value m a
runModuleTable = raiseHandler $ runReader lowerBound

runImportGraphToModuleInfos :: ( AnalyzeTerm term
                               , HasPrelude lang
                               , Has Trace sig m
                               , Effect sig
                               )
                            => Proxy lang
                            -> Package (term Loc)
                            -> m (Graph ControlFlowVertex)
runImportGraphToModuleInfos lang package = runImportGraph lang package allModuleInfos
  where allModuleInfos info = vertex (maybe (unknownModuleVertex info) (moduleVertex . moduleInfo) (ModuleTable.lookup (modulePath info) (packageModules package)))

runImportGraphToModules :: ( AnalyzeTerm term
                           , HasPrelude lang
                           , Has Trace sig m
                           , Effect sig
                           )
                        => Proxy lang
                        -> Package (term Loc)
                        -> m (Graph (Module (term Loc)))
runImportGraphToModules lang package = runImportGraph lang package resolveOrLowerBound
  where resolveOrLowerBound info = maybe lowerBound vertex (ModuleTable.lookup (modulePath info) (packageModules package))

runImportGraph :: ( AnalyzeTerm term
                  , HasPrelude lang
                  , Has Trace sig m
                  , Effect sig
                  )
               => Proxy lang
               -> Package (term Loc)
               -> (ModuleInfo -> Graph vertex)
               -> m (Graph vertex)
runImportGraph lang package f
  = fmap (fst >=> f)
  . runEvaluator @_ @_ @(Value _ (Hole (Maybe Name) Precise))
  . raiseHandler (runState lowerBound)
  . runHeap
  . raiseHandler (runFresh 0)
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
  . raiseHandler (runState (lowerBound @(ScopeGraph (Hole (Maybe Name) Precise))))
  . runAllocator
  $ evaluate lang (graphingModuleInfo (runDomainEffects (evalTerm id))) (snd <$> ModuleTable.toPairs (packageModules package))

runHeap :: Evaluator term address value (StateC (Heap address address value) m) a
        -> Evaluator term address value m (Heap address address value, a)
runHeap = raiseHandler (runState lowerBound)

runScopeGraph :: Ord address
              => Evaluator term address value (StateC (ScopeGraph address) m) a
              -> Evaluator term address value m (ScopeGraph address, a)
runScopeGraph = raiseHandler (runState lowerBound)

-- | Parse a list of files into a 'Package'.
parsePackage :: (Has Distribute sig m, Has Resolution sig m, Has Parse sig m, Has Trace sig m)
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
parseModules :: (Has Distribute sig m, Has Parse sig m) => Parser term -> Project -> m [Module (Blob, term)]
parseModules parser p = distributeFor (projectBlobs p) (parseModule p parser)


-- | Parse a list of packages from a python project.
parsePythonPackage :: forall term sig m .
                   ( AnalyzeTerm term
                   , Has Distribute sig m
                   , Has Parse sig m
                   , Has Resolution sig m
                   , Has Trace sig m
                   , Effect sig
                   )
                   => Parser (term Loc) -- ^ A parser.
                   -> Project           -- ^ Project to parse into a package.
                   -> m (Package (term Loc))
parsePythonPackage parser project = do
  let runAnalysis = runEvaluator @_ @_ @(Value (term Loc) (Hole (Maybe Name) Precise))
        . raiseHandler (runState PythonPackage.Unknown)
        . raiseHandler (runState (lowerBound @(Heap (Hole (Maybe Name) Precise) (Hole (Maybe Name) Precise) (Value (term Loc) (Hole (Maybe Name) Precise)))))
        . raiseHandler (runFresh 0)
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
        . raiseHandler (runState (lowerBound @(ScopeGraph (Hole (Maybe Name) Precise))))
        . runAllocator

  strat <- case find (\b -> blobPath b == (projectRootDir project </> "setup.py")) (projectBlobs project) of
    Just setupFile -> do
      setupModule <- fmap snd <$> parseModule project parser setupFile
      fst <$> runAnalysis (evaluate (Proxy @'Language.Python) (runDomainEffects (runPythonPackaging . evalTerm id)) [ setupModule ])
    Nothing -> pure PythonPackage.Unknown
  case strat of
    PythonPackage.Unknown -> do
      modules <- fmap (fmap snd) <$> parseModules parser project
      resMap <- Task.resolutionMap project
      pure (Package.fromModules (Data.Abstract.Evaluatable.name (projectName project)) modules resMap) -- TODO: Confirm this is the right `name`.
    PythonPackage.Packages dirs ->
      packageFromProject project [ blob | dir <- dirs
                                        , blob <- projectBlobs project
                                        , packageDir <- [projectRootDir project </> unpack dir]
                                        , packageDir `isPrefixOf` blobPath blob
                                        ]
    PythonPackage.FindPackages excludeDirs -> do
      trace "In Graph.FindPackages"
      let initFiles = filter (isInit . filePath) (projectFiles project)
          isInit = (== Path.relFile "__init__.py") . Path.takeFileName
          packageDirs = filter (`notElem` ((projectRootDir project </>) . unpack <$> excludeDirs)) (takeDirectory . Path.toString . filePath <$> initFiles)
      packageFromProject project [ blob | dir <- packageDirs
                                        , blob <- projectBlobs project
                                        , dir `isPrefixOf` blobPath blob
                                        ]
    where
      packageFromProject project filteredBlobs = do
        let p = project { projectBlobs = filteredBlobs }
        modules <- fmap (fmap snd) <$> parseModules parser p
        resMap <- Task.resolutionMap p
        pure (Package.fromModules (Data.Abstract.Evaluatable.name $ projectName p) modules resMap) -- TODO: Confirm this is the right `name`.

parseModule :: Has Parse sig m
            => Project
            -> Parser term
            -> Blob
            -> m (Module (Blob, term))
parseModule proj parser blob = moduleForBlob (Just (projectRootDir proj)) blob . (,) blob <$> parse parser blob

withTermSpans :: ( Has (Reader Span) sig m
                 , Has (State Span) sig m -- last evaluated child's span
                 )
              => (term -> Span)
              -> Open (term -> Evaluator term address value m a)
withTermSpans getSpan recur term = let
  span = getSpan term
  updatedSpanAlg = withCurrentSpan span (recur term)
  in modifyChildSpan span updatedSpanAlg

resumingResolutionError :: ( Has Trace sig m
                           )
                         => Evaluator term address value (ResumableC (BaseError ResolutionError) m) a
                         -> Evaluator term address value m a
resumingResolutionError = runResolutionErrorWith $ \ baseError -> do
  traceError "ResolutionError" baseError
  case baseErrorException baseError of
    NotFoundError nameToResolve _ _ -> pure nameToResolve
    GoImportError pathToResolve     -> pure [pathToResolve]

resumingLoadError :: ( Has Trace sig m
                     , AbstractHole value
                     , AbstractHole address
                     )
                  => Evaluator term address value (ResumableC (BaseError (LoadError address value)) m) a
                  -> Evaluator term address value m a
resumingLoadError = runLoadErrorWith (\ baseError -> traceError "LoadError" baseError *> case baseErrorException baseError of
  ModuleNotFoundError _ -> pure ((hole, hole), hole))

resumingEvalError :: ( Has Fresh sig m
                     , Has Trace sig m
                     , Show value
                     , Show term
                     , AbstractHole address
                     , AbstractHole value
                     )
                  => Evaluator term address value (ResumableC (BaseError (EvalError term address value)) m) a
                  -> Evaluator term address value m a
resumingEvalError = runEvalErrorWith (\ baseError -> traceError "EvalError" baseError *> case baseErrorException baseError of
  AccessControlError{}  -> pure hole
  ConstructorError{}    -> pure hole
  DefaultExportError{}  -> pure ()
  DerefError{}          -> pure hole
  ExportError{}         -> pure ()
  FloatFormatError{}    -> pure 0
  IntegerFormatError{}  -> pure 0
  NoNameError{}         -> gensym
  RationalFormatError{} -> pure 0
  ReferenceError{}      -> pure hole
  ScopedEnvError{}      -> pure hole)

resumingUnspecialized :: ( AbstractHole address
                         , AbstractHole value
                         , Has Trace sig m
                         )
                      => Evaluator term address value (ResumableC (BaseError (UnspecializedError address value)) m) a
                      -> Evaluator term address value m a
resumingUnspecialized = runUnspecializedWith (\ baseError -> traceError "UnspecializedError" baseError *> case baseErrorException baseError of
  UnspecializedError _    -> pure hole
  RefUnspecializedError _ -> pure hole)

resumingAddressError :: ( AbstractHole value
                        , Has Trace sig m
                        , Show address
                        )
                     => Evaluator term address value (ResumableC (BaseError (AddressError address value)) m) a
                     -> Evaluator term address value m a
resumingAddressError = runAddressErrorWith $ \ baseError -> do
  traceError "AddressError" baseError
  case baseErrorException baseError of
    UnallocatedSlot   _ -> pure lowerBound
    UninitializedSlot _ -> pure hole

resumingValueError :: ( Has Trace sig m
                      , Show address
                      , Show term
                      )
                   => Evaluator term address (Value term address) (ResumableC (BaseError (ValueError term address)) m) a
                   -> Evaluator term address (Value term address) m a
resumingValueError = runValueErrorWith (\ baseError -> traceError "ValueError" baseError *> case baseErrorException baseError of
  CallError{}       -> pure hole
  StringError val   -> pure (pack (prettyShow val))
  BoolError{}       -> pure True
  BoundsError{}     -> pure hole
  IndexError{}      -> pure hole
  NumericError{}    -> pure hole
  Numeric2Error{}   -> pure hole
  ComparisonError{} -> pure hole
  BitwiseError{}    -> pure hole
  Bitwise2Error{}   -> pure hole
  KeyValueError{}   -> pure (hole, hole)
  ArrayError{}      -> pure lowerBound
  ArithmeticError{} -> pure hole)

resumingHeapError :: ( AbstractHole address
                     , Has Trace sig m
                     , Show address
                     )
                  => Evaluator term address value (ResumableC (BaseError (HeapError address)) m) a
                  -> Evaluator term address value m a
resumingHeapError = runHeapErrorWith (\ baseError -> traceError "ScopeError" baseError *> case baseErrorException baseError of
    CurrentFrameError     -> pure hole
    LookupAddressError _  -> pure hole
    -- FIXME: this is clearly bogus
    LookupFrameError addr -> pure (Frame addr lowerBound lowerBound)
    LookupLinksError _    -> pure mempty
    LookupLinkError _     -> pure hole)

resumingScopeError :: ( Has Trace sig m
                      , AbstractHole (Slot address)
                      , AbstractHole (Scope address)
                      , AbstractHole (Path address)
                      , AbstractHole (Info address)
                      , AbstractHole address
                      )
                    => Evaluator term address value (ResumableC (BaseError (ScopeError address)) m) a
                    -> Evaluator term address value m a
resumingScopeError = runScopeErrorWith (\ baseError -> traceError "ScopeError" baseError *> case baseErrorException baseError of
  ScopeError _ _                -> pure hole
  ImportReferenceError          -> pure hole
  LookupScopeError              -> pure hole
  LookupPathError _             -> pure hole
  CurrentScopeError             -> pure hole
  LookupDeclarationScopeError _ -> pure hole
  DeclarationByNameError _      -> pure hole)

resumingTypeError :: ( Has Trace sig m
                     , Effect sig
                     , Alternative m
                     )
                  => Evaluator term address Type (ResumableC (BaseError TypeError)
                                                 (StateC TypeMap
                                                 m)) a
                  -> Evaluator term address Type m a
resumingTypeError = runTypesWith (\ baseError -> traceError "TypeError" baseError *> case baseErrorException baseError of
  UnificationError l r -> pure l <|> pure r
  InfiniteType _ r     -> pure r)

prettyShow :: Show a => a -> String
prettyShow = hscolour TTY defaultColourPrefs False False "" False . ppShow

traceError :: (Has Trace sig m, Show (exc resume)) => String -> BaseError exc resume -> Evaluator term address value m ()
traceError prefix baseError = trace $ prefix <> ": " <> prettyShow baseError
