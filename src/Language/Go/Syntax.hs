{-# LANGUAGE DeriveAnyClass, DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Go.Syntax where

import Prologue

import Control.Abstract
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.Package as Package
import           Data.Abstract.Path
import           Data.JSON.Fields
import qualified Data.Map as Map
import           Data.Semigroup.App
import           Data.Semigroup.Foldable
import qualified Data.Text as T
import           Diffing.Algorithm
import           Proto3.Suite.Class
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import Data.ImportPath
import           System.FilePath.Posix

resolveGoImport :: ( Member (Modules address value) sig
                   , Member (Reader ModuleInfo) sig
                   , Member (Reader Package.PackageInfo) sig
                   , Member (Reader Span) sig
                   , Member (Resumable (BaseError ResolutionError)) sig
                   , Member Trace sig
                   , Carrier sig m
                   )
                => ImportPath
                -> Evaluator term address value m [ModulePath]
resolveGoImport (ImportPath path Unknown) = throwResolutionError $ GoImportError path
resolveGoImport (ImportPath path Relative) = do
  ModuleInfo{..} <- currentModule
  paths <- listModulesInDir (joinPaths (takeDirectory modulePath) path)
  case paths of
    [] -> throwResolutionError $ GoImportError path
    _ -> pure paths
resolveGoImport (ImportPath path NonRelative) = do
  package <- T.unpack . formatName . Package.packageName <$> currentPackage
  trace ("attempting to resolve " <> show path <> " for package " <> package)
  case splitDirectories path of
    -- Import an absolute path that's defined in this package being analyzed.
    -- First two are source, next is package name, remaining are path to package
    -- (e.g. github.com/golang/<package>/path...).
    (_ : _ : p : xs) | p == package -> listModulesInDir (joinPath xs)
    _  -> throwResolutionError $ GoImportError path

-- | Import declarations (symbols are added directly to the calling environment).
--
-- If the list of symbols is empty copy everything to the calling environment.
data Import a = Import { importFrom :: ImportPath, importWildcardToken :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Import

instance Evaluatable Import where
  eval _ _ (Language.Go.Syntax.Import importPath _) = do
    paths <- resolveGoImport importPath
    for_ paths $ \path -> do
      traceResolve (unPath importPath) path
      ((moduleScope, moduleFrame), _) <- require path
      insertImportEdge moduleScope
      insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
    unit


-- | Qualified Import declarations (symbols are qualified in calling environment).
--
-- If the list of symbols is empty copy and qualify everything to the calling environment.
data QualifiedImport a = QualifiedImport { qualifiedImportFrom :: !ImportPath, qualifiedImportAlias :: !a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedImport

instance Evaluatable QualifiedImport where
  eval _ _ (QualifiedImport importPath aliasTerm) = do
    paths <- resolveGoImport importPath
    alias <- maybeM (throwNoNameError aliasTerm) (declaredName aliasTerm)
    span <- ask @Span
    scopeAddress <- newScope mempty
    declare (Declaration alias) Default span ScopeGraph.QualifiedImport (Just scopeAddress)
    aliasSlot <- lookupDeclaration (Declaration alias)

    withScope scopeAddress $ do
      let
        go [] = pure ()
        go (modulePath : paths) =
          mkScopeMap modulePath (\scopeMap -> do
            objFrame <- newFrame scopeAddress (Map.singleton ScopeGraph.Import scopeMap)
            val <- object objFrame
            assign aliasSlot val
            for_ paths $ \modulePath ->
              mkScopeMap modulePath (withFrame objFrame . insertFrameLink ScopeGraph.Import))
          where mkScopeMap modulePath fun = do
                  ((moduleScope, moduleFrame), _) <- require modulePath
                  insertImportEdge moduleScope
                  fun (Map.singleton moduleScope moduleFrame)
      go paths
    unit

-- | Side effect only imports (no symbols made available to the calling environment).
data SideEffectImport a = SideEffectImport { sideEffectImportFrom :: !ImportPath, sideEffectImportToken :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically SideEffectImport

-- TODO: Revisit this and confirm if this is correct.
instance Evaluatable SideEffectImport where
  eval _ _ (SideEffectImport importPath _) = do
    paths <- resolveGoImport importPath
    traceResolve (unPath importPath) paths
    for_ paths $ \path -> require path -- Do we need to construct any scope / frames for these side-effect imports?
    unit

-- A composite literal in Go
data Composite a = Composite { compositeType :: !a, compositeElement :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Composite

-- TODO: Implement Eval instance for Composite
instance Evaluatable Composite

-- | A default pattern in a Go select or switch statement (e.g. `switch { default: s() }`).
newtype DefaultPattern a = DefaultPattern { defaultPatternBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically DefaultPattern

-- TODO: Implement Eval instance for DefaultPattern
instance Evaluatable DefaultPattern

-- | A defer statement in Go (e.g. `defer x()`).
newtype Defer a = Defer { deferBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Defer

-- TODO: Implement Eval instance for Defer
instance Evaluatable Defer

-- | A go statement (i.e. go routine) in Go (e.g. `go x()`).
newtype Go a = Go { goBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Go

-- TODO: Implement Eval instance for Go
instance Evaluatable Go

-- | A label statement in Go (e.g. `label:continue`).
data Label a = Label { labelName :: !a, labelStatement :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Label

-- TODO: Implement Eval instance for Label
instance Evaluatable Label

-- | A rune literal in Go (e.g. `'⌘'`).
newtype Rune a = Rune { runeLiteral :: Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Rune

-- TODO: Implement Eval instance for Rune
instance Evaluatable Rune

-- | A select statement in Go (e.g. `select { case x := <-c: x() }` where each case is a send or receive operation on channels).
newtype Select a = Select { selectCases :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Select

-- TODO: Implement Eval instance for Select
instance Evaluatable Select

-- | A send statement in Go (e.g. `channel <- value`).
data Send a = Send { sendReceiver :: !a, sendValue :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Send

-- TODO: Implement Eval instance for Send
instance Evaluatable Send

-- | A slice expression in Go (e.g. `a[1:4:3]` where a is a list, 1 is the low bound, 4 is the high bound, and 3 is the max capacity).
data Slice a = Slice { sliceName :: !a, sliceLow :: !a, sliceHigh :: !a, sliceCapacity :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Slice

-- TODO: Implement Eval instance for Slice
instance Evaluatable Slice

-- | A type switch statement in Go (e.g. `switch x.(type) { // cases }`).
data TypeSwitch a = TypeSwitch { typeSwitchSubject :: !a, typeSwitchCases :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeSwitch

-- TODO: Implement Eval instance for TypeSwitch
instance Evaluatable TypeSwitch

-- | A type switch guard statement in a Go type switch statement (e.g. `switch i := x.(type) { // cases}`).
newtype TypeSwitchGuard a = TypeSwitchGuard { typeSwitchGuardSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeSwitchGuard

-- TODO: Implement Eval instance for TypeSwitchGuard
instance Evaluatable TypeSwitchGuard

-- | A receive statement in a Go select statement (e.g. `case value := <-channel` )
data Receive a = Receive { receiveSubject :: !a, receiveExpression :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Receive

-- TODO: Implement Eval instance for Receive
instance Evaluatable Receive

-- | A receive operator unary expression in Go (e.g. `<-channel` )
newtype ReceiveOperator a = ReceiveOperator { value :: a}
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ReceiveOperator

-- TODO: Implement Eval instance for ReceiveOperator
instance Evaluatable ReceiveOperator

-- | A field declaration in a Go struct type declaration.
data Field a = Field { fieldContext :: ![a], fieldName :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Field

-- TODO: Implement Eval instance for Field
instance Evaluatable Field

data Package a = Package { packageName :: !a, packageContents :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Package

instance Evaluatable Package where
  eval eval _ (Package _ xs) = maybe unit (runApp . foldMap1 (App . eval)) (nonEmpty xs)

-- | A type assertion in Go (e.g. `x.(T)` where the value of `x` is not nil and is of type `T`).
data TypeAssertion a = TypeAssertion { typeAssertionSubject :: !a, typeAssertionType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeAssertion

-- TODO: Implement Eval instance for TypeAssertion
instance Evaluatable TypeAssertion

-- | A type conversion expression in Go (e.g. `T(x)` where `T` is a type and `x` is an expression that can be converted to type `T`).
data TypeConversion a = TypeConversion { typeConversionType :: !a, typeConversionSubject :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeConversion

-- TODO: Implement Eval instance for TypeConversion
instance Evaluatable TypeConversion

-- | Variadic arguments and parameters in Go (e.g. parameter: `param ...Type`, argument: `Type...`).
data Variadic a = Variadic { variadicContext :: [a], variadicIdentifier :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Named1, Message1, NFData1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically Variadic

-- TODO: Implement Eval instance for Variadic
instance Evaluatable Variadic
