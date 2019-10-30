{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, FlexibleContexts #-}
module Language.PHP.Syntax (module Language.PHP.Syntax) where

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
import           Diffing.Algorithm
import           Source.Span

newtype Text a = Text { value :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Text where liftEq = genericLiftEq
instance Ord1 Text where liftCompare = genericLiftCompare
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Text

newtype VariableName a = VariableName { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 VariableName where liftEq = genericLiftEq
instance Ord1 VariableName where liftCompare = genericLiftCompare
instance Show1 VariableName where liftShowsPrec = genericLiftShowsPrec

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
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Require where liftEq = genericLiftEq
instance Ord1 Require where liftCompare = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Require where
  eval eval _ (Require path) = include eval path load


newtype RequireOnce a = RequireOnce { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 RequireOnce where liftEq = genericLiftEq
instance Ord1 RequireOnce where liftCompare = genericLiftCompare
instance Show1 RequireOnce where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable RequireOnce where
  eval eval _ (RequireOnce path) = include eval path require

newtype Include a = Include { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Include where liftEq = genericLiftEq
instance Ord1 Include where liftCompare = genericLiftCompare
instance Show1 Include where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Include where
  eval eval _ (Include path) = include eval path load

newtype IncludeOnce a = IncludeOnce { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 IncludeOnce where liftEq = genericLiftEq
instance Ord1 IncludeOnce where liftCompare = genericLiftCompare
instance Show1 IncludeOnce where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable IncludeOnce where
  eval eval _ (IncludeOnce path) = include eval path require

newtype ArrayElement a = ArrayElement { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ArrayElement where liftEq = genericLiftEq
instance Ord1 ArrayElement where liftCompare = genericLiftCompare
instance Show1 ArrayElement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ArrayElement

newtype GlobalDeclaration a = GlobalDeclaration { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 GlobalDeclaration where liftEq = genericLiftEq
instance Ord1 GlobalDeclaration where liftCompare = genericLiftCompare
instance Show1 GlobalDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GlobalDeclaration

newtype SimpleVariable a = SimpleVariable { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 SimpleVariable where liftEq = genericLiftEq
instance Ord1 SimpleVariable where liftCompare = genericLiftCompare
instance Show1 SimpleVariable where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable SimpleVariable

data Concat a = Concat { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Concat where liftEq = genericLiftEq
instance Ord1 Concat where liftCompare = genericLiftCompare
instance Show1 Concat where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Concat

-- | TODO: Unify with TypeScript's PredefinedType
newtype CastType a = CastType { _castType :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 CastType where liftEq = genericLiftEq
instance Ord1 CastType where liftCompare = genericLiftCompare
instance Show1 CastType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable CastType

newtype ErrorControl a = ErrorControl { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ErrorControl where liftEq = genericLiftEq
instance Ord1 ErrorControl where liftCompare = genericLiftCompare
instance Show1 ErrorControl where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ErrorControl

newtype Clone a = Clone { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Clone where liftEq = genericLiftEq
instance Ord1 Clone where liftCompare = genericLiftCompare
instance Show1 Clone where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Clone

newtype ShellCommand a = ShellCommand { value :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ShellCommand where liftEq = genericLiftEq
instance Ord1 ShellCommand where liftCompare = genericLiftCompare
instance Show1 ShellCommand where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ShellCommand

-- | TODO: Combine with TypeScript update expression.
newtype Update a = Update { _updateSubject :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Update where liftEq = genericLiftEq
instance Ord1 Update where liftCompare = genericLiftCompare
instance Show1 Update where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable Update

newtype NewVariable a = NewVariable { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NewVariable where liftEq = genericLiftEq
instance Ord1 NewVariable where liftCompare = genericLiftCompare
instance Show1 NewVariable where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NewVariable

newtype RelativeScope a = RelativeScope { value :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 RelativeScope where liftEq = genericLiftEq
instance Ord1 RelativeScope where liftCompare = genericLiftCompare
instance Show1 RelativeScope where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable RelativeScope

data QualifiedName a = QualifiedName { name :: a, identifier :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 QualifiedName where liftEq = genericLiftEq
instance Ord1 QualifiedName where liftCompare = genericLiftCompare
instance Show1 QualifiedName where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable QualifiedName where
  eval _ _ (QualifiedName obj iden) = do
    -- TODO: Consider gensym'ed names used for References.
    name <- maybeM (throwNoNameError obj) (declaredName obj)
    reference (Reference name) (obj^.span_) ScopeGraph.Identifier (Declaration name)
    childScope <- associatedScope (Declaration name)

    propName <- maybeM (throwNoNameError iden) (declaredName iden)
    case childScope of
      Just childScope -> do
        currentScopeAddress <- currentScope
        currentFrameAddress <- currentFrame
        frameAddress <- newFrame childScope (Map.singleton Lexical (Map.singleton currentScopeAddress currentFrameAddress))
        withScopeAndFrame frameAddress $ do
          reference (Reference propName) (iden^.span_) ScopeGraph.Identifier (Declaration propName)
          slot <- lookupSlot (Declaration propName)
          deref slot
      Nothing ->
        -- TODO: Throw an ReferenceError because we can't find the associated child scope for `obj`.
        unit

newtype NamespaceName a = NamespaceName { names :: NonEmpty a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, ToJSONFields1, Traversable)

instance Eq1 NamespaceName where liftEq = genericLiftEq
instance Ord1 NamespaceName where liftCompare = genericLiftCompare
instance Show1 NamespaceName where liftShowsPrec = genericLiftShowsPrec

instance Hashable1 NamespaceName where liftHashWithSalt = foldl

instance Evaluatable NamespaceName

newtype ConstDeclaration a = ConstDeclaration { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ConstDeclaration where liftEq = genericLiftEq
instance Ord1 ConstDeclaration where liftCompare = genericLiftCompare
instance Show1 ConstDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstDeclaration

data ClassConstDeclaration a = ClassConstDeclaration { visibility :: a, elements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ClassConstDeclaration where liftEq = genericLiftEq
instance Ord1 ClassConstDeclaration where liftCompare = genericLiftCompare
instance Show1 ClassConstDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ClassConstDeclaration

newtype ClassInterfaceClause a = ClassInterfaceClause { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ClassInterfaceClause where liftEq = genericLiftEq
instance Ord1 ClassInterfaceClause where liftCompare = genericLiftCompare
instance Show1 ClassInterfaceClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ClassInterfaceClause

newtype ClassBaseClause a = ClassBaseClause { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ClassBaseClause where liftEq = genericLiftEq
instance Ord1 ClassBaseClause where liftCompare = genericLiftCompare
instance Show1 ClassBaseClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ClassBaseClause

newtype UseClause a = UseClause { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 UseClause where liftEq = genericLiftEq
instance Ord1 UseClause where liftCompare = genericLiftCompare
instance Show1 UseClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable UseClause

newtype ReturnType a = ReturnType { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ReturnType where liftEq = genericLiftEq
instance Ord1 ReturnType where liftCompare = genericLiftCompare
instance Show1 ReturnType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ReturnType

newtype TypeDeclaration a = TypeDeclaration { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 TypeDeclaration where liftEq = genericLiftEq
instance Ord1 TypeDeclaration where liftCompare = genericLiftCompare
instance Show1 TypeDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeDeclaration

newtype BaseTypeDeclaration a = BaseTypeDeclaration { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 BaseTypeDeclaration where liftEq = genericLiftEq
instance Ord1 BaseTypeDeclaration where liftCompare = genericLiftCompare
instance Show1 BaseTypeDeclaration where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable BaseTypeDeclaration

newtype ScalarType a = ScalarType { value :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ScalarType where liftEq = genericLiftEq
instance Ord1 ScalarType where liftCompare = genericLiftCompare
instance Show1 ScalarType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ScalarType

newtype EmptyIntrinsic a = EmptyIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 EmptyIntrinsic where liftEq = genericLiftEq
instance Ord1 EmptyIntrinsic where liftCompare = genericLiftCompare
instance Show1 EmptyIntrinsic where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable EmptyIntrinsic

newtype ExitIntrinsic a = ExitIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ExitIntrinsic where liftEq = genericLiftEq
instance Ord1 ExitIntrinsic where liftCompare = genericLiftCompare
instance Show1 ExitIntrinsic where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ExitIntrinsic

newtype IssetIntrinsic a = IssetIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 IssetIntrinsic where liftEq = genericLiftEq
instance Ord1 IssetIntrinsic where liftCompare = genericLiftCompare
instance Show1 IssetIntrinsic where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable IssetIntrinsic

newtype EvalIntrinsic a = EvalIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 EvalIntrinsic where liftEq = genericLiftEq
instance Ord1 EvalIntrinsic where liftCompare = genericLiftCompare
instance Show1 EvalIntrinsic where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable EvalIntrinsic

newtype PrintIntrinsic a = PrintIntrinsic { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 PrintIntrinsic where liftEq = genericLiftEq
instance Ord1 PrintIntrinsic where liftCompare = genericLiftCompare
instance Show1 PrintIntrinsic where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PrintIntrinsic

newtype NamespaceAliasingClause a = NamespaceAliasingClause { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NamespaceAliasingClause where liftEq = genericLiftEq
instance Ord1 NamespaceAliasingClause where liftCompare = genericLiftCompare
instance Show1 NamespaceAliasingClause where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable NamespaceAliasingClause

newtype NamespaceUseDeclaration a = NamespaceUseDeclaration { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NamespaceUseDeclaration where liftEq = genericLiftEq
instance Ord1 NamespaceUseDeclaration where liftCompare = genericLiftCompare
instance Show1 NamespaceUseDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NamespaceUseDeclaration

newtype NamespaceUseClause a = NamespaceUseClause { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NamespaceUseClause where liftEq = genericLiftEq
instance Ord1 NamespaceUseClause where liftCompare = genericLiftCompare
instance Show1 NamespaceUseClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NamespaceUseClause

newtype NamespaceUseGroupClause a = NamespaceUseGroupClause { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 NamespaceUseGroupClause where liftEq = genericLiftEq
instance Ord1 NamespaceUseGroupClause where liftCompare = genericLiftCompare
instance Show1 NamespaceUseGroupClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable NamespaceUseGroupClause

data Namespace a = Namespace { namespaceName :: [a], namespaceBody :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Namespace where liftEq = genericLiftEq
instance Ord1 Namespace where liftCompare = genericLiftCompare
instance Show1 Namespace where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Namespace

data TraitDeclaration a = TraitDeclaration { traitName :: a, traitStatements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 TraitDeclaration where liftEq = genericLiftEq
instance Ord1 TraitDeclaration where liftCompare = genericLiftCompare
instance Show1 TraitDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TraitDeclaration

data AliasAs a = AliasAs { aliasAsName  :: a, aliasAsModifier :: a, aliasAsClause :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 AliasAs where liftEq = genericLiftEq
instance Ord1 AliasAs where liftCompare = genericLiftCompare
instance Show1 AliasAs where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AliasAs

data InsteadOf a = InsteadOf { left :: a, right :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 InsteadOf where liftEq = genericLiftEq
instance Ord1 InsteadOf where liftCompare = genericLiftCompare
instance Show1 InsteadOf where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InsteadOf

newtype TraitUseSpecification a = TraitUseSpecification { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 TraitUseSpecification where liftEq = genericLiftEq
instance Ord1 TraitUseSpecification where liftCompare = genericLiftCompare
instance Show1 TraitUseSpecification where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TraitUseSpecification

data TraitUseClause a = TraitUseClause { namespace :: [a], alias :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 TraitUseClause where liftEq = genericLiftEq
instance Ord1 TraitUseClause where liftCompare = genericLiftCompare
instance Show1 TraitUseClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TraitUseClause

data DestructorDeclaration a = DestructorDeclaration { body:: [a], name :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 DestructorDeclaration where liftEq = genericLiftEq
instance Ord1 DestructorDeclaration where liftCompare = genericLiftCompare
instance Show1 DestructorDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DestructorDeclaration

newtype Static a = Static { value :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Static where liftEq = genericLiftEq
instance Ord1 Static where liftCompare = genericLiftCompare
instance Show1 Static where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Static

newtype ClassModifier a = ClassModifier { value :: T.Text }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ClassModifier where liftEq = genericLiftEq
instance Ord1 ClassModifier where liftCompare = genericLiftCompare
instance Show1 ClassModifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ClassModifier

data ConstructorDeclaration a = ConstructorDeclaration { modifiers :: [a], parameters :: [a], body :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 ConstructorDeclaration where liftEq = genericLiftEq
instance Ord1 ConstructorDeclaration where liftCompare = genericLiftCompare
instance Show1 ConstructorDeclaration where liftShowsPrec = genericLiftShowsPrec
instance Evaluatable ConstructorDeclaration

data PropertyDeclaration a = PropertyDeclaration { modifier :: a, elements :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 PropertyDeclaration where liftEq = genericLiftEq
instance Ord1 PropertyDeclaration where liftCompare = genericLiftCompare
instance Show1 PropertyDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PropertyDeclaration

data PropertyModifier a = PropertyModifier { visibility :: a , static :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 PropertyModifier where liftEq = genericLiftEq
instance Ord1 PropertyModifier where liftCompare = genericLiftCompare
instance Show1 PropertyModifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PropertyModifier

data InterfaceDeclaration a = InterfaceDeclaration { name :: a, base :: a, declarations :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 InterfaceDeclaration where liftEq = genericLiftEq
instance Ord1 InterfaceDeclaration where liftCompare = genericLiftCompare
instance Show1 InterfaceDeclaration where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InterfaceDeclaration

newtype InterfaceBaseClause a = InterfaceBaseClause { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 InterfaceBaseClause where liftEq = genericLiftEq
instance Ord1 InterfaceBaseClause where liftCompare = genericLiftCompare
instance Show1 InterfaceBaseClause where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InterfaceBaseClause

newtype Echo a = Echo { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Echo where liftEq = genericLiftEq
instance Ord1 Echo where liftCompare = genericLiftCompare
instance Show1 Echo where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Echo

newtype Unset a = Unset { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Unset where liftEq = genericLiftEq
instance Ord1 Unset where liftCompare = genericLiftCompare
instance Show1 Unset where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Unset

data Declare a = Declare { left :: a, right :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Declare where liftEq = genericLiftEq
instance Ord1 Declare where liftCompare = genericLiftCompare
instance Show1 Declare where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Declare

newtype DeclareDirective a = DeclareDirective { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 DeclareDirective where liftEq = genericLiftEq
instance Ord1 DeclareDirective where liftCompare = genericLiftCompare
instance Show1 DeclareDirective where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DeclareDirective

newtype LabeledStatement a = LabeledStatement { _labeledStatementIdentifier :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 LabeledStatement where liftEq = genericLiftEq
instance Ord1 LabeledStatement where liftCompare = genericLiftCompare
instance Show1 LabeledStatement where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LabeledStatement
