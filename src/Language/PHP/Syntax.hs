{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.PHP.Syntax where

import Prelude hiding (span)
import Prologue hiding (Text)

import           Control.Lens.Getter
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import           Control.Abstract as Abstract
import           Data.Abstract.BaseError
import           Data.Abstract.Evaluatable as Abstract
import           Data.Abstract.Module
import           Data.Abstract.Path
import qualified Data.Abstract.ScopeGraph as ScopeGraph
import           Data.JSON.Fields
import qualified Data.Language as Language
import           Data.Span
import           Diffing.Algorithm

newtype Text a = Text { value :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Text

instance Evaluatable Text

newtype VariableName a = VariableName { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically VariableName

instance Evaluatable VariableName

-- TODO: Variables defined in an included file take on scope of the source line
-- on which the inclusion occurs in the including file. However, functions and
-- classes defined in the included file are always in global scope.

-- TODO: If inclusion occurs inside a function definition within the including
-- file, the complete contents of the included file are treated as though it
-- were defined inside that function.

resolvePHPName :: ( Member (Modules address value) sig
                  , Member (Reader ModuleInfo) sig
                  , Member (Reader Span) sig
                  , Member (Resumable (BaseError ResolutionError)) sig
                  , Carrier sig m
                  )
               => T.Text
               -> Evaluator term address value m ModulePath
resolvePHPName n = do
  modulePath <- resolve [name]
  maybeM (throwResolutionError $ NotFoundError name [name] Language.PHP) modulePath
  where name = toName n
        toName = T.unpack . dropRelativePrefix . stripQuotes

include :: ( Carrier sig m
           , Member (Modules address value) sig
           , Member (Reader (CurrentFrame address)) sig
           , Member (Reader (CurrentScope address)) sig
           , Member (Reader ModuleInfo) sig
           , Member (Reader Span) sig
           , Member (Resumable (BaseError (HeapError address))) sig
           , Member (State (ScopeGraph address)) sig
           , Member (Resumable (BaseError ResolutionError)) sig
           , Member (State (Heap address address value)) sig
           , Member (Abstract.String value) sig
           , Member Trace sig
           , Ord address
           )
        => (term -> Evaluator term address value m value)
        -> term
        -> (ModulePath -> Evaluator term address value m (ModuleResult address value))
        -> Evaluator term address value m value
include eval pathTerm f = do
  name <- eval pathTerm >>= asString
  path <- resolvePHPName name
  traceResolve name path
  ((moduleScope, moduleFrame), v) <- f path
  insertImportEdge moduleScope
  insertFrameLink ScopeGraph.Import (Map.singleton moduleScope moduleFrame)
  pure v

newtype Require a = Require { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Require

instance Evaluatable Require where
  eval eval _ (Require path) = include eval path load


newtype RequireOnce a = RequireOnce { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically RequireOnce

instance Evaluatable RequireOnce where
  eval eval _ (RequireOnce path) = include eval path require

newtype Include a = Include { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Include

instance Evaluatable Include where
  eval eval _ (Include path) = include eval path load

newtype IncludeOnce a = IncludeOnce { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically IncludeOnce

instance Evaluatable IncludeOnce where
  eval eval _ (IncludeOnce path) = include eval path require

newtype ArrayElement a = ArrayElement { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ArrayElement

instance Evaluatable ArrayElement

newtype GlobalDeclaration a = GlobalDeclaration { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically GlobalDeclaration

instance Evaluatable GlobalDeclaration

newtype SimpleVariable a = SimpleVariable { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically SimpleVariable

instance Evaluatable SimpleVariable

data Concat a = Concat { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Concat

instance Evaluatable Concat

-- | TODO: Unify with TypeScript's PredefinedType
newtype CastType a = CastType { _castType :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically CastType

instance Evaluatable CastType

newtype ErrorControl a = ErrorControl { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ErrorControl

instance Evaluatable ErrorControl

newtype Clone a = Clone { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Clone

instance Evaluatable Clone

newtype ShellCommand a = ShellCommand { value :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ShellCommand

instance Evaluatable ShellCommand

-- | TODO: Combine with TypeScript update expression.
newtype Update a = Update { _updateSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Update
instance Evaluatable Update

newtype NewVariable a = NewVariable { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NewVariable

instance Evaluatable NewVariable

newtype RelativeScope a = RelativeScope { value :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically RelativeScope

instance Evaluatable RelativeScope

data QualifiedName a = QualifiedName { name :: a, identifier :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedName

instance Evaluatable QualifiedName where
  eval _ _ (QualifiedName obj iden) = do
    -- TODO: Consider gensym'ed names used for References.
    name <- maybeM (throwNoNameError obj) (declaredName obj)
    reference (Reference name) (obj^.span) ScopeGraph.Identifier (Declaration name)
    childScope <- associatedScope (Declaration name)

    propName <- maybeM (throwNoNameError iden) (declaredName iden)
    case childScope of
      Just childScope -> do
        currentScopeAddress <- currentScope
        currentFrameAddress <- currentFrame
        frameAddress <- newFrame childScope (Map.singleton Lexical (Map.singleton currentScopeAddress currentFrameAddress))
        withScopeAndFrame frameAddress $ do
          reference (Reference propName) (iden^.span) ScopeGraph.Identifier (Declaration propName)
          slot <- lookupSlot (Declaration propName)
          deref slot
      Nothing ->
        -- TODO: Throw an ReferenceError because we can't find the associated child scope for `obj`.
        unit

newtype NamespaceName a = NamespaceName { names :: NonEmpty a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Diffable, FreeVariables1, Declarations1, ToJSONFields1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NamespaceName

instance Hashable1 NamespaceName where liftHashWithSalt = foldl

instance Evaluatable NamespaceName

newtype ConstDeclaration a = ConstDeclaration { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ConstDeclaration

instance Evaluatable ConstDeclaration

data ClassConstDeclaration a = ClassConstDeclaration { visibility :: a, elements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ClassConstDeclaration

instance Evaluatable ClassConstDeclaration

newtype ClassInterfaceClause a = ClassInterfaceClause { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ClassInterfaceClause

instance Evaluatable ClassInterfaceClause

newtype ClassBaseClause a = ClassBaseClause { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ClassBaseClause

instance Evaluatable ClassBaseClause

newtype UseClause a = UseClause { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically UseClause

instance Evaluatable UseClause

newtype ReturnType a = ReturnType { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ReturnType

instance Evaluatable ReturnType

newtype TypeDeclaration a = TypeDeclaration { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeDeclaration

instance Evaluatable TypeDeclaration

newtype BaseTypeDeclaration a = BaseTypeDeclaration { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically BaseTypeDeclaration
instance Evaluatable BaseTypeDeclaration

newtype ScalarType a = ScalarType { value :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ScalarType

instance Evaluatable ScalarType

newtype EmptyIntrinsic a = EmptyIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically EmptyIntrinsic

instance Evaluatable EmptyIntrinsic

newtype ExitIntrinsic a = ExitIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ExitIntrinsic

instance Evaluatable ExitIntrinsic

newtype IssetIntrinsic a = IssetIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically IssetIntrinsic

instance Evaluatable IssetIntrinsic

newtype EvalIntrinsic a = EvalIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically EvalIntrinsic

instance Evaluatable EvalIntrinsic

newtype PrintIntrinsic a = PrintIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PrintIntrinsic

instance Evaluatable PrintIntrinsic

newtype NamespaceAliasingClause a = NamespaceAliasingClause { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NamespaceAliasingClause
instance Evaluatable NamespaceAliasingClause

newtype NamespaceUseDeclaration a = NamespaceUseDeclaration { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NamespaceUseDeclaration

instance Evaluatable NamespaceUseDeclaration

newtype NamespaceUseClause a = NamespaceUseClause { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NamespaceUseClause

instance Evaluatable NamespaceUseClause

newtype NamespaceUseGroupClause a = NamespaceUseGroupClause { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically NamespaceUseGroupClause

instance Evaluatable NamespaceUseGroupClause

data Namespace a = Namespace { namespaceName :: [a], namespaceBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Namespace

instance Evaluatable Namespace

data TraitDeclaration a = TraitDeclaration { traitName :: a, traitStatements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TraitDeclaration

instance Evaluatable TraitDeclaration

data AliasAs a = AliasAs { aliasAsName  :: a, aliasAsModifier :: a, aliasAsClause :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically AliasAs

instance Evaluatable AliasAs

data InsteadOf a = InsteadOf { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InsteadOf

instance Evaluatable InsteadOf

newtype TraitUseSpecification a = TraitUseSpecification { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TraitUseSpecification

instance Evaluatable TraitUseSpecification

data TraitUseClause a = TraitUseClause { namespace :: [a], alias :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TraitUseClause

instance Evaluatable TraitUseClause

data DestructorDeclaration a = DestructorDeclaration { body:: [a], name :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically DestructorDeclaration

instance Evaluatable DestructorDeclaration

newtype Static a = Static { value :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Static

instance Evaluatable Static

newtype ClassModifier a = ClassModifier { value :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ClassModifier

instance Evaluatable ClassModifier

data ConstructorDeclaration a = ConstructorDeclaration { modifiers :: [a], parameters :: [a], body :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ConstructorDeclaration
instance Evaluatable ConstructorDeclaration

data PropertyDeclaration a = PropertyDeclaration { modifier :: a, elements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PropertyDeclaration

instance Evaluatable PropertyDeclaration

data PropertyModifier a = PropertyModifier { visibility :: a , static :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PropertyModifier

instance Evaluatable PropertyModifier

data InterfaceDeclaration a = InterfaceDeclaration { name :: a, base :: a, declarations :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InterfaceDeclaration

instance Evaluatable InterfaceDeclaration

newtype InterfaceBaseClause a = InterfaceBaseClause { values :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InterfaceBaseClause

instance Evaluatable InterfaceBaseClause

newtype Echo a = Echo { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Echo

instance Evaluatable Echo

newtype Unset a = Unset { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Unset

instance Evaluatable Unset

data Declare a = Declare { left :: a, right :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Declare

instance Evaluatable Declare

newtype DeclareDirective a = DeclareDirective { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically DeclareDirective

instance Evaluatable DeclareDirective

newtype LabeledStatement a = LabeledStatement { _labeledStatementIdentifier :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LabeledStatement

instance Evaluatable LabeledStatement
