{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.Python.Syntax where

import           Data.Abstract.BaseError
import           Data.Abstract.Environment as Env
import           Data.Abstract.Evaluatable hiding (Import)
import           Data.Abstract.Module
import           Data.Aeson
import           Data.Functor.Classes.Generic
import           Data.JSON.Fields
import qualified Data.Language as Language
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import           Diffing.Algorithm
import           GHC.Generics
import           Prologue
import           System.FilePath.Posix
import Proto3.Suite (Primitive(..), Message(..), Message1(..), Named1(..), Named(..), MessageField(..), DotProtoIdentifier(..), DotProtoPrimType(..), DotProtoType(..), messageField)
import qualified Proto3.Suite as Proto
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Wire.Decode as Decode
import Control.Abstract.ScopeGraph (Allocator, bindAll, insertImportEdge, declare, Declaration(..))
import qualified Data.Abstract.ScopeGraph as ScopeGraph

data QualifiedName
  = QualifiedName { paths :: NonEmpty FilePath }
  | RelativeQualifiedName { path :: FilePath, maybeQualifiedName ::  Maybe QualifiedName }
  deriving (Eq, Generic, Hashable, Ord, Show, ToJSON, Named, Message, NFData)

instance MessageField QualifiedName where
  encodeMessageField num QualifiedName{..} = Encode.embedded num (encodeMessageField 1 paths)
  encodeMessageField num RelativeQualifiedName{..} = Encode.embedded num (encodeMessageField 1 path <> encodeMessageField 2 maybeQualifiedName)
  decodeMessageField = Decode.embedded'' (qualifiedName <|> relativeQualifiedName)
    where
      qualifiedName = QualifiedName <$> Decode.at decodeMessageField 1
      relativeQualifiedName = RelativeQualifiedName <$> Decode.at decodeMessageField 1 <*> Decode.at decodeMessageField 2
  protoType _ = messageField (Prim $ Named (Single (nameOf (Proxy @QualifiedName)))) Nothing

qualifiedName :: NonEmpty Text -> QualifiedName
qualifiedName xs = QualifiedName (T.unpack <$> xs)

relativeQualifiedName :: Text -> [Text] -> QualifiedName
relativeQualifiedName prefix []    = RelativeQualifiedName (T.unpack prefix) Nothing
relativeQualifiedName prefix paths = RelativeQualifiedName (T.unpack prefix) (Just (qualifiedName (NonEmpty.fromList paths)))

-- Python module resolution.
-- https://docs.python.org/3/reference/import.html#importsystem
--
-- TODO: Namespace packages
--
-- Regular packages resolution:
--
-- parent/
--     __init__.py
--     one/
--         __init__.py
--     two/
--         __init__.py
--     three/
--         __init__.py
--
-- `import parent.one` will implicitly execute:
--     `parent/__init__.py` and
--     `parent/one/__init__.py`
-- Subsequent imports of `parent.two` or `parent.three` will execute
--     `parent/two/__init__.py` and
--     `parent/three/__init__.py` respectively.
resolvePythonModules :: ( Member (Modules address value) sig
                        , Member (Reader ModuleInfo) sig
                        , Member (Reader Span) sig
                        , Member (Resumable (BaseError ResolutionError)) sig
                        , Member Trace sig
                        , Carrier sig m
                        )
                     => QualifiedName
                     -> Evaluator term address value m (NonEmpty ModulePath)
resolvePythonModules q = do
  relRootDir <- rootDir q <$> currentModule
  for (moduleNames q) $ \name -> do
    x <- search relRootDir name
    x <$ traceResolve name x
  where
    rootDir (QualifiedName _) ModuleInfo{..}           = mempty -- overall rootDir of the Package.
    rootDir (RelativeQualifiedName n _) ModuleInfo{..} = upDir numDots (takeDirectory modulePath)
      where numDots = pred (length n)
            upDir n dir | n <= 0 = dir
                        | otherwise = takeDirectory (upDir (pred n) dir)

    moduleNames (QualifiedName qualifiedName)          = NonEmpty.scanl1 (</>) qualifiedName
    moduleNames (RelativeQualifiedName x Nothing)      = error $ "importing from '" <> show x <> "' is not implemented"
    moduleNames (RelativeQualifiedName _ (Just paths)) = moduleNames paths

    search rootDir x = do
      trace ("searching for " <> show x <> " in " <> show rootDir)
      let path = normalise (rootDir </> normalise x)
      let searchPaths = [ path </> "__init__.py"
                        , path <.> ".py"
                        ]
      modulePath <- resolve searchPaths
      maybeM (throwResolutionError $ NotFoundError path searchPaths Language.Python) modulePath


-- | Import declarations (symbols are added directly to the calling environment).
--
-- If the list of symbols is empty copy everything to the calling environment.
data Import a = Import { importFrom :: QualifiedName, importSymbols :: ![Alias] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)

instance Eq1 Import where liftEq = genericLiftEq
instance Ord1 Import where liftCompare = genericLiftCompare
instance Show1 Import where liftShowsPrec = genericLiftShowsPrec

newtype FutureImport a = FutureImport { futureImportSymbols :: [Alias] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)

instance Eq1 FutureImport where liftEq = genericLiftEq
instance Ord1 FutureImport where liftCompare = genericLiftCompare
instance Show1 FutureImport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FutureImport where

data Alias = Alias { aliasValue :: Name, aliasName :: Name }
  deriving (Eq, Generic, Hashable, Ord, Show, Message, Named, ToJSON, NFData)

toTuple :: Alias -> (Name, Name)
toTuple Alias{..} = (aliasValue, aliasName)


-- from a import b
instance Evaluatable Import where
  -- from . import moduleY
  -- This is a bit of a special case in the syntax as this actually behaves like a qualified relative import.
  eval _ (Import (RelativeQualifiedName n Nothing) [Alias{..}]) = do
    path <- NonEmpty.last <$> resolvePythonModules (RelativeQualifiedName n (Just (qualifiedName (formatName aliasValue :| []))))
    scopeGraph <- fst <$> require path
    bindAll scopeGraph
    span <- ask @Span
    declare (Declaration aliasValue) span (ScopeGraph.currentScope scopeGraph)
    rvalBox unit

  -- from a import b
  -- from a import b as c
  -- from a import *
  -- from .moduleY import b
  eval _ (Import name xs) = do
    modulePaths <- resolvePythonModules name

    -- Eval parent modules first
    for_ (NonEmpty.init modulePaths) require

    -- Last module path is the one we want to import
    let path = NonEmpty.last modulePaths
    scopeGraph <- fst <$> require path
    bindAll scopeGraph
    if Prologue.null xs then
      maybe (pure ()) insertImportEdge (ScopeGraph.currentScope scopeGraph)
    else
      for_ xs $ \Alias{..} -> do
        -- TODO: Add an Alias Edge to resolve qualified export froms
        -- Scope 1 -> alias (bar, foo) -> Export 3 -> Export -> Scope 4
        pure ()

    rvalBox unit
    -- where
    --   select importedBinds
    --     | Prologue.null xs = importedBinds
    --     | otherwise = Env.aliasBindings (toTuple <$> xs) importedBinds


-- -- Evaluate a qualified import
-- evalQualifiedImport :: ( AbstractValue term address value m
--                        , Carrier sig m
--                        , Member (Allocator address) sig
--                        , Member (Deref value) sig
--                        , Member (Modules address value) sig
--                        , Member (State (Heap address address value)) sig
--                        , Ord address
--                        )
--                     => Name -> ModulePath -> Evaluator term address value m value
-- evalQualifiedImport name path = letrec' name $ \addr -> do
--   unit <$ makeNamespace name addr Nothing (bindAll . fst . snd =<< require path)

newtype QualifiedImport a = QualifiedImport { qualifiedImportFrom :: NonEmpty FilePath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)

instance Message1 QualifiedImport where
  liftEncodeMessage _ _ QualifiedImport{..} = encodeMessageField 1 qualifiedImportFrom
  liftDecodeMessage _ _ = QualifiedImport <$> Decode.at decodeMessageField 1
  liftDotProto _ = [ Proto.DotProtoMessageField $ Proto.DotProtoField 1 (Repeated Proto.String) (Single "qualifiedImportFrom") [] Nothing ]

instance Named Prelude.String where nameOf _ = "string"

instance Message Prelude.String where
  encodeMessage _ = encodePrimitive 1
  decodeMessage _ = Decode.at (Decode.one decodePrimitive mempty) 1
  dotProto = undefined

instance Eq1 QualifiedImport where liftEq = genericLiftEq
instance Ord1 QualifiedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedImport where liftShowsPrec = genericLiftShowsPrec

-- import a.b.c
instance Evaluatable QualifiedImport where
  eval _ (QualifiedImport qualifiedName) = do
    modulePaths <- resolvePythonModules (QualifiedName qualifiedName)
    -- rvalBox =<< go (NonEmpty.zip (Data.Abstract.Evaluatable.name . T.pack <$> qualifiedName) modulePaths)
    rvalBox unit
    where
      -- -- Evaluate and import the last module, updating the environment
      -- go ((name, path) :| []) = evalQualifiedImport name path
      -- -- Evaluate each parent module, just creating a namespace
      -- go ((name, path) :| xs) = letrec' name $ \addr -> do
      --   void $ require path
      --   makeNamespace name addr Nothing (void (require path >> go (NonEmpty.fromList xs)))

data QualifiedAliasedImport a = QualifiedAliasedImport { qualifiedAliasedImportFrom :: QualifiedName, qualifiedAliasedImportAlias :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)

instance Eq1 QualifiedAliasedImport where liftEq = genericLiftEq
instance Ord1 QualifiedAliasedImport where liftCompare = genericLiftCompare
instance Show1 QualifiedAliasedImport where liftShowsPrec = genericLiftShowsPrec

-- import a.b.c as e
instance Evaluatable QualifiedAliasedImport where
  eval _ (QualifiedAliasedImport name aliasTerm) = do
    modulePaths <- resolvePythonModules name

    -- Evaluate each parent module
    for_ (NonEmpty.init modulePaths) require

    -- Evaluate and import the last module, aliasing and updating the environment
    alias <- maybeM (throwEvalError NoNameError) (declaredName aliasTerm)
    -- rvalBox =<< letrec' alias (\addr -> do
    --   let path = NonEmpty.last modulePaths
    --   unit <$ makeNamespace alias addr Nothing (void (bindAll . fst . snd =<< require path)))
    undefined

-- | Ellipsis (used in splice expressions and alternatively can be used as a fill in expression, like `undefined` in Haskell)
data Ellipsis a = Ellipsis
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)

instance Eq1 Ellipsis where liftEq = genericLiftEq
instance Ord1 Ellipsis where liftCompare = genericLiftCompare
instance Show1 Ellipsis where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Ellipsis
instance Evaluatable Ellipsis


data Redirect a = Redirect { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)

instance Eq1 Redirect where liftEq = genericLiftEq
instance Ord1 Redirect where liftCompare = genericLiftCompare
instance Show1 Redirect where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Redirect
instance Evaluatable Redirect
