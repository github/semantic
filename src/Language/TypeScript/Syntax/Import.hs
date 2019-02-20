{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TypeScript.Syntax.Import where

import Prologue

import           Control.Abstract hiding (Import)
import           Data.Abstract.Evaluatable as Evaluatable
import qualified Data.Abstract.Name as Name
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.JSON.Fields
import           Diffing.Algorithm
import           Language.TypeScript.Resolution
import Data.Span (emptySpan)
import qualified Data.Map.Strict as Map
import Data.Aeson (ToJSON)

data Import a = Import { importSymbols :: ![Alias], importFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Import

  -- http://www.typescriptlang.org/docs/handbook/module-resolution.html
instance Evaluatable Import where
  eval _ _ (Import symbols importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    ((moduleScope, moduleFrame), _) <- require modulePath
    if Prologue.null symbols then do
      insertImportEdge moduleScope
      insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    else do
      let scopeEdges = Map.singleton ScopeGraph.Import [ moduleScope ]
      scopeAddress <- newScope scopeEdges
      let frameLinks = Map.singleton ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
      frameAddress <- newFrame scopeAddress frameLinks

      -- Insert import references into the import scope starting from the perspective of the import scope.
      withScopeAndFrame moduleFrame $ do
        for_ symbols $ \Alias{..} ->
          -- TODO: Need an easier way to get the span of an Alias. It's difficult because we no longer have a term.
          -- Even if we had one we'd have to evaluate it at the moment.
          insertImportReference (Reference aliasName) emptySpan ScopeGraph.Identifier (Declaration aliasValue) scopeAddress

      -- Create edges from the current scope/frame to the import scope/frame.
      insertImportEdge scopeAddress
      insertFrameLink ScopeGraph.Import (Map.singleton scopeAddress frameAddress)
    unit

data QualifiedAliasedImport a = QualifiedAliasedImport { qualifiedAliasedImportAlias :: !a, qualifiedAliasedImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedAliasedImport

instance Evaluatable QualifiedAliasedImport where
  eval _ _ (QualifiedAliasedImport aliasTerm importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    ((moduleScope, moduleFrame), _) <- require modulePath
    span <- ask @Span

    importScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])
    let scopeMap = Map.singleton moduleScope moduleFrame
    aliasFrame <- newFrame importScope (Map.singleton ScopeGraph.Import scopeMap)

    alias <- maybeM (throwNoNameError aliasTerm) (declaredName aliasTerm)
    declare (Declaration alias) Default Public span ScopeGraph.QualifiedAliasedImport (Just importScope)
    aliasSlot <- lookupSlot (Declaration alias)
    assign aliasSlot =<< object aliasFrame

    unit

newtype SideEffectImport a = SideEffectImport { sideEffectImportFrom :: ImportPath }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically SideEffectImport

instance Evaluatable SideEffectImport where
  eval _ _ (SideEffectImport importPath) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions
    void $ require modulePath
    unit

-- | Qualified Export declarations
newtype QualifiedExport a = QualifiedExport { qualifiedExportSymbols :: [Alias] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedExport

instance Evaluatable QualifiedExport where
  eval _ _ (QualifiedExport exportSymbols) = do
    -- Create a Lexical edge from the qualifed export's scope to the current scope.
    currentScopeAddress <- currentScope
    let edges = Map.singleton Lexical [ currentScopeAddress ]
    exportScope <- newScope edges
    insertExportEdge exportScope -- Create an export edge from the current scope to the export scope
    withScope exportScope .
      for_ exportSymbols $ \Alias{..} -> do
        -- TODO: Replace Alias in QualifedExport with terms and use a real span
        reference (Reference aliasName) emptySpan ScopeGraph.Identifier (Declaration aliasValue)

    -- Create an export edge from a new scope to the qualifed export's scope.
    unit

data Alias = Alias { aliasValue :: Name, aliasName :: Name }
  deriving (Eq, Generic, Hashable, Ord, Show, ToJSON, NFData)

toTuple :: Alias -> (Name, Name)
toTuple Alias{..} = (aliasValue, aliasName)

-- | Qualified Export declarations that export from another module.
data QualifiedExportFrom a = QualifiedExportFrom { qualifiedExportFrom :: ImportPath, qualifiedExportFromSymbols :: ![Alias]}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedExportFrom

instance Evaluatable QualifiedExportFrom where
  eval _ _ (QualifiedExportFrom importPath exportSymbols) = do
    modulePath <- resolveWithNodejsStrategy importPath typescriptExtensions

    ((moduleScope, moduleFrame), _) <- require modulePath
    exportScope <- newScope (Map.singleton ScopeGraph.Import [ moduleScope ])
    exportFrame <- newFrame exportScope (Map.singleton ScopeGraph.Import (Map.singleton moduleScope moduleFrame))

    withScopeAndFrame moduleFrame .
      for_ exportSymbols $ \Alias{..} -> do
        -- TODO: Replace Alias with terms in QualifiedExportFrom and use a real span below.
        insertImportReference (Reference aliasName) emptySpan ScopeGraph.Identifier (Declaration aliasValue) exportScope

    insertExportEdge exportScope
    insertFrameLink ScopeGraph.Export (Map.singleton exportScope exportFrame)

    unit

newtype DefaultExport a = DefaultExport { defaultExport :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically DefaultExport

instance Evaluatable DefaultExport where
  eval eval _ (DefaultExport term) = do
    case declaredName term of
      Just _ -> do
        exportScope <- newScope mempty
        exportFrame <- newFrame exportScope mempty
        exportSpan <- ask @Span
        withScopeAndFrame exportFrame $ do
          valueRef <- eval term
          let declaration = Declaration $ Name.name "__default"
          declare declaration Default Public exportSpan ScopeGraph.DefaultExport Nothing
          defaultSlot <- lookupSlot declaration
          assign defaultSlot valueRef

        insertExportEdge exportScope
        insertFrameLink ScopeGraph.Export (Map.singleton exportScope exportFrame)
      Nothing -> throwEvalError DefaultExportError
    unit

data ImportRequireClause a = ImportRequireClause { importRequireIdentifier :: !a, importRequireSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ImportRequireClause

instance Evaluatable ImportRequireClause

newtype ImportClause a = ImportClause { importClauseElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ImportClause

instance Evaluatable ImportClause

data ImportAlias a = ImportAlias { importAliasSubject :: !a, importAlias :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ImportAlias

instance Evaluatable ImportAlias
