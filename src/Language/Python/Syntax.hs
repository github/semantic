{-# LANGUAGE DeriveAnyClass, DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- FIXME
module Language.Python.Syntax where

import           Control.Abstract.Heap
import           Control.Abstract.ScopeGraph hiding (Import)
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.Aeson hiding (object)
import           Data.Functor.Classes.Generic
import           Data.JSON.Fields
import qualified Data.Language as Language
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Diffing.Algorithm
import           GHC.Generics
import           Prologue
import           Proto3.Suite
    ( DotProtoIdentifier (..)
    , DotProtoPrimType (..)
    , DotProtoType (..)
    , Message (..)
    , Message1 (..)
    , MessageField (..)
    , Named (..)
    , Named1 (..)
    , Primitive (..)
    , messageField
    )
import qualified Proto3.Wire.Decode as Decode
import qualified Proto3.Wire.Encode as Encode
import           System.FilePath.Posix

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
  for (moduleNames q) $ \relPath -> do
    x <- search relRootDir relPath
    x <$ traceResolve relPath x
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

data Alias a = Alias { aliasValue :: a, aliasName :: a}
  deriving (Generic1, Diffable, Eq, Foldable, FreeVariables1, Functor, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Alias

instance Declarations1 Alias where
  liftDeclaredName declaredName = declaredName . aliasValue
  liftDeclaredAlias declaredAlias = declaredAlias . aliasName

instance Evaluatable Alias where
  eval eval _ Alias{..} = eval aliasValue
  ref _ ref Alias{..} = ref aliasValue

-- | Import declarations (symbols are added directly to the calling environment).
--
-- If the list of symbols is empty copy everything to the calling environment.
data Import a = Import { importFrom :: QualifiedName, importSymbols :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Import

newtype FutureImport a = FutureImport { futureImportSymbols :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FutureImport

instance Evaluatable FutureImport where


-- from a import b
instance Evaluatable Import where
  -- from . import moduleY            -- aliasValue = moduleY, aliasName = moduleY
  -- from . import moduleY as moduleZ -- aliasValue = moduleY, aliasName = moduleZ
  -- This is a bit of a special case in the syntax as this actually behaves like a qualified relative import.
  eval eval _ (Import (RelativeQualifiedName n Nothing) [aliasTerm]) = do
    aliasValue' <- maybeM (throwNoNameError aliasTerm) (declaredName aliasTerm)
    path <- NonEmpty.last <$> resolvePythonModules (RelativeQualifiedName n (Just (qualifiedName (formatName aliasValue' :| []))))
    ((moduleScope, moduleFrame), _) <- require path

    -- Construct a proxy scope containing an import edge to the imported module's last returned scope.
    importScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])

    -- Construct an object frame.
    let scopeMap = Map.singleton moduleScope moduleFrame
    aliasFrame <- newFrame importScope (Map.singleton ScopeGraph.Import scopeMap)

    -- Add declaration of the alias name to the current scope (within our current module).
    aliasName <- maybeM (throwNoNameError aliasTerm) (declaredAlias aliasTerm)
    _ <- eval aliasTerm
    aliasSpan <- get @Span
    declare (Declaration aliasName) Default Public aliasSpan ScopeGraph.UnqualifiedImport (Just importScope)
    -- Retrieve the frame slot for the new declaration.
    aliasSlot <- lookupSlot (Declaration aliasName)
    assign aliasSlot =<< object aliasFrame

    unit

  -- from a import b
  -- from a import b as c
  -- from a import *
  -- from .moduleY import b
  eval eval _ (Import name xs) = do
    modulePaths <- resolvePythonModules name

    -- Eval parent modules first
    for_ (NonEmpty.init modulePaths) require

    -- Last module path is the one we want to import
    let path = NonEmpty.last modulePaths
    ((moduleScope, moduleFrame), _) <- require path
    if Prologue.null xs then do
      insertImportEdge moduleScope
      insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    else do
      let scopeEdges = Map.singleton ScopeGraph.Import [ moduleScope ]
      scopeAddress <- newScope scopeEdges
      withScope moduleScope .
        for_ xs $ \aliasTerm -> do
          aliasValue <- maybeM (throwNoNameError aliasTerm) (declaredName aliasTerm)
          aliasName <- maybeM (throwNoNameError aliasTerm) (declaredAlias aliasTerm)
          _ <- eval aliasTerm
          aliasSpan <- get @Span
          if aliasValue /= aliasName then
            insertImportReference (Reference aliasName) aliasSpan ScopeGraph.Identifier (Declaration aliasValue) scopeAddress
          else pure ()

      let frameLinks = Map.singleton moduleScope moduleFrame
      frameAddress <- newFrame scopeAddress (Map.singleton ScopeGraph.Import frameLinks)

      insertImportEdge scopeAddress
      insertFrameLink ScopeGraph.Import (Map.singleton scopeAddress frameAddress)

    unit

deriving instance Hashable1 NonEmpty

newtype QualifiedImport a = QualifiedImport { qualifiedImportFrom :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1, Message1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedImport

instance Named Prelude.String where nameOf _ = "string"

instance Message Prelude.String where
  encodeMessage _ = encodePrimitive 1
  decodeMessage _ = Decode.at (Decode.one decodePrimitive mempty) 1
  dotProto = undefined

-- import a.b.c
instance Evaluatable QualifiedImport where
  eval eval _ (QualifiedImport qualifiedNames) = do
    qualifiedName <- fmap (T.unpack . formatName) <$> traverse (\term -> maybeM (throwNoNameError term) (declaredName term)) qualifiedNames
    modulePaths <- resolvePythonModules (QualifiedName qualifiedName)
    let namesAndPaths = toList (NonEmpty.zip (NonEmpty.zip qualifiedNames (Data.Abstract.Evaluatable.name . T.pack <$> qualifiedName)) modulePaths)

    go namesAndPaths
    unit
    where
      go [] = pure ()
      go (((nameTerm, name), modulePath) : namesAndPaths) = do
        _ <- eval nameTerm
        span <- get @Span
        scopeAddress <- newScope mempty
        declare (Declaration name) Default Public span ScopeGraph.QualifiedImport (Just scopeAddress)
        aliasSlot <- lookupSlot (Declaration name)
        -- a.b.c
        withScope scopeAddress $
          mkScopeMap modulePath (\scopeMap -> do
              objFrame <- newFrame scopeAddress (Map.singleton ScopeGraph.Import scopeMap)
              val <- object objFrame
              assign aliasSlot val

              withFrame objFrame $ do
                let (namePaths, rest) = List.partition ((== name) . snd . fst) namesAndPaths
                for_ namePaths $ \(_, modulePath) -> do
                  mkScopeMap modulePath $ \scopeMap -> do
                    withFrame objFrame $ do
                      insertFrameLink ScopeGraph.Import scopeMap
                go rest)
      mkScopeMap modulePath fun = do
        ((moduleScope, moduleFrame), _) <- require modulePath
        insertImportEdge moduleScope
        fun (Map.singleton moduleScope moduleFrame)

data QualifiedAliasedImport a = QualifiedAliasedImport { qualifiedAliasedImportFrom :: QualifiedName, qualifiedAliasedImportAlias :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedAliasedImport

-- import a.b.c as e
instance Evaluatable QualifiedAliasedImport where
  eval _ _ (QualifiedAliasedImport name aliasTerm) = do
    modulePaths <- resolvePythonModules name

    span <- ask @Span
    scopeAddress <- newScope mempty
    alias <- maybeM (throwNoNameError aliasTerm) (declaredName aliasTerm)
    declare (Declaration alias) Default Public span ScopeGraph.QualifiedAliasedImport (Just scopeAddress)
    objFrame <- newFrame scopeAddress mempty
    val <- object objFrame
    aliasSlot <- lookupSlot (Declaration alias)
    assign aliasSlot val

    withScopeAndFrame objFrame .
      for_ modulePaths $ \modulePath -> do
        ((moduleScope, moduleFrame), _) <- require modulePath
        insertImportEdge moduleScope
        insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)

    unit

-- | Ellipsis (used in splice expressions and alternatively can be used as a fill in expression, like `undefined` in Haskell)
data Ellipsis a = Ellipsis
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Ellipsis

-- TODO: Implement Eval instance for Ellipsis
instance Evaluatable Ellipsis

data Redirect a = Redirect { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Message1, Named1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Redirect

-- TODO: Implement Eval instance for Redirect
instance Evaluatable Redirect
