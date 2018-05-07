{-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
module Control.Abstract.ModuleTable
( ModuleTable
, getModuleTable
, putModuleTable
, modifyModuleTable
, resolve
, listModulesInDir
, require
, load
, LoadError(..)
, runLoadError
, runLoadErrorWith
, ResolutionError(..)
, runResolutionError
, runResolutionErrorWith
) where

import Control.Abstract.Environment
import Control.Abstract.Evaluator
import Control.Abstract.Exports
import Data.Abstract.Environment
import Data.Abstract.Exports as Exports
import Data.Abstract.Module
import Data.Abstract.ModuleTable as ModuleTable
import Data.Language
import Prologue

-- | Retrieve the table of evaluated modules.
getModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => Evaluator location term value effects (ModuleTable (Environment location value, value))
getModuleTable = raise get

-- | Set the table of evaluated modules.
putModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => ModuleTable (Environment location value, value) -> Evaluator location term value effects ()
putModuleTable = raise . put

-- | Update the evaluated module table.
modifyModuleTable :: Member (State (ModuleTable (Environment location value, value))) effects => (ModuleTable (Environment location value, value) -> ModuleTable (Environment location value, value)) -> Evaluator location term value effects ()
modifyModuleTable = raise . modify'


-- | Retrieve the table of unevaluated modules.
askModuleTable :: Member (Reader (ModuleTable [Module term])) effects
               => Evaluator location term value effects (ModuleTable [Module term])
askModuleTable = raise ask


-- | Retrieve the module load stack
askLoadStack :: Member (Reader LoadStack) effects => Evaluator location term value effects LoadStack
askLoadStack = raise ask

-- | Locally update the module load stack.
localLoadStack :: Member (Reader LoadStack) effects => (LoadStack -> LoadStack) -> Evaluator location term value effects a -> Evaluator location term value effects a
localLoadStack = raiseHandler . local


-- Resolve a list of module paths to a possible module table entry.
resolve :: Member (Reader (ModuleTable [Module term])) effects
        => [FilePath]
        -> Evaluator location term value effects (Maybe ModulePath)
resolve names = do
  tbl <- askModuleTable
  pure $ find (`ModuleTable.member` tbl) names

listModulesInDir :: Member (Reader (ModuleTable [Module term])) effects
                 => FilePath
                 -> Evaluator location term value effects [ModulePath]
listModulesInDir dir = modulePathsInDir dir <$> askModuleTable


-- | Require/import another module by name and return it's environment and value.
--
-- Looks up the term's name in the cache of evaluated modules first, returns if found, otherwise loads/evaluates the module.
require :: Members '[ EvalModule term value
                    , Reader (ModuleTable [Module term])
                    , Reader LoadStack
                    , Resumable (LoadError term)
                    , State (Environment location value)
                    , State (Exports location value)
                    , State (ModuleTable (Environment location value, value))
                    ] effects
        => ModulePath
        -> Evaluator location term value effects (Maybe (Environment location value, value))
require name = getModuleTable >>= maybeM (load name) . fmap Just . ModuleTable.lookup name

-- | Load another module by name and return it's environment and value.
--
-- Always loads/evaluates.
load :: Members '[ EvalModule term value
                 , Reader (ModuleTable [Module term])
                 , Reader LoadStack
                 , Resumable (LoadError term)
                 , State (Environment location value)
                 , State (Exports location value)
                 , State (ModuleTable (Environment location value, value))
                 ] effects
     => ModulePath
     -> Evaluator location term value effects (Maybe (Environment location value, value))
load name = askModuleTable >>= maybeM notFound . ModuleTable.lookup name >>= runMerging . foldMap (Merging . evalAndCache)
  where
    notFound = throwResumable (LoadError name)

    evalAndCache x = do
      let mPath = modulePath (moduleInfo x)
      LoadStack{..} <- askLoadStack
      if moduleInfo x `elem` unLoadStack
        then trace ("load (skip evaluating, circular load): " <> show mPath) (pure Nothing)
        else do
          v <- localLoadStack (loadStackPush (moduleInfo x)) (trace ("load (evaluating): " <> show mPath) (evaluateModule x))
          traceM ("load done:" <> show mPath)
          env <- filterEnv <$> getExports <*> getEnv
          modifyModuleTable (ModuleTable.insert name (env, v))
          pure (Just (env, v))

    -- TODO: If the set of exports is empty because no exports have been
    -- defined, do we export all terms, or no terms? This behavior varies across
    -- languages. We need better semantics rather than doing it ad-hoc.
    filterEnv :: Exports.Exports l a -> Environment l a -> Environment l a
    filterEnv ports env
      | null ports = env
      | otherwise  = Exports.toEnvironment ports `mergeEnvs` overwrite (Exports.aliases ports) env

newtype Merging m location value = Merging { runMerging :: m (Maybe (Environment location value, value)) }

instance Applicative m => Semigroup (Merging m location value) where
  Merging a <> Merging b = Merging (merge <$> a <*> b)
    where merge a b = mergeJusts <$> a <*> b <|> a <|> b
          mergeJusts (env1, _) (env2, v) = (mergeEnvs env1 env2, v)

instance Applicative m => Monoid (Merging m location value) where
  mappend = (<>)
  mempty = Merging (pure Nothing)


-- | An error thrown when loading a module from the list of provided modules. Indicates we weren't able to find a module with the given name.
data LoadError term resume where
  LoadError :: ModulePath -> LoadError term [Module term]

deriving instance Eq (LoadError term resume)
deriving instance Show (LoadError term resume)
instance Show1 (LoadError term) where
  liftShowsPrec _ _ = showsPrec
instance Eq1 (LoadError term) where
  liftEq _ (LoadError a) (LoadError b) = a == b

runLoadError :: Evaluator location term value (Resumable (LoadError term) ': effects) a -> Evaluator location term value effects (Either (SomeExc (LoadError term)) a)
runLoadError = raiseHandler runError

runLoadErrorWith :: (forall resume . LoadError term resume -> Evaluator location term value effects resume) -> Evaluator location term value (Resumable (LoadError term) ': effects) a -> Evaluator location term value effects a
runLoadErrorWith = runResumableWith


-- | An error thrown when we can't resolve a module from a qualified name.
data ResolutionError resume where
  NotFoundError :: String   -- ^ The path that was not found.
                -> [String] -- ^ List of paths searched that shows where semantic looked for this module.
                -> Language -- ^ Language.
                -> ResolutionError ModulePath

  GoImportError :: FilePath -> ResolutionError [ModulePath]

deriving instance Eq (ResolutionError b)
deriving instance Show (ResolutionError b)
instance Show1 ResolutionError where liftShowsPrec _ _ = showsPrec
instance Eq1 ResolutionError where
  liftEq _ (NotFoundError a _ l1) (NotFoundError b _ l2) = a == b && l1 == l2
  liftEq _ (GoImportError a) (GoImportError b) = a == b
  liftEq _ _ _ = False

runResolutionError :: Effectful m => m (Resumable ResolutionError ': effects) a -> m effects (Either (SomeExc ResolutionError) a)
runResolutionError = raiseHandler runError

runResolutionErrorWith :: Effectful m => (forall resume . ResolutionError resume -> m effects resume) -> m (Resumable ResolutionError ': effects) a -> m effects a
runResolutionErrorWith = runResumableWith
