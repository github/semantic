{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.TypeScript.Syntax.Types where

import Prologue

import           Control.Abstract hiding (Import)
import           Data.Abstract.Evaluatable as Evaluatable
import           Data.JSON.Fields
import qualified Data.Text as T
import           Diffing.Algorithm
import qualified Data.Abstract.ScopeGraph as ScopeGraph

-- | Lookup type for a type-level key in a typescript map.
data LookupType a = LookupType { lookupTypeIdentifier :: a, lookupTypeKey :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically LookupType

instance Evaluatable LookupType

data FunctionType a = FunctionType { functionTypeParameters :: !a, functionFormalParameters :: ![a], functionType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically FunctionType

instance Evaluatable FunctionType

data TypeParameter a = TypeParameter { typeParameter :: !a, typeParameterConstraint :: !a, typeParameterDefaultType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeParameter

instance Evaluatable TypeParameter

data TypeAssertion a = TypeAssertion { typeAssertionParameters :: !a, typeAssertionExpression :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeAssertion

instance Evaluatable TypeAssertion

newtype DefaultType a = DefaultType { defaultType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically DefaultType

instance Evaluatable DefaultType

newtype ParenthesizedType a = ParenthesizedType { parenthesizedType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ParenthesizedType

instance Evaluatable ParenthesizedType

newtype PredefinedType a = PredefinedType { predefinedType :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically PredefinedType

-- TODO: Implement Eval instance for PredefinedType
instance Evaluatable PredefinedType

newtype TypeIdentifier a = TypeIdentifier { contents :: T.Text }
  deriving (Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeIdentifier

instance Declarations1 TypeIdentifier where
  liftDeclaredName _ (TypeIdentifier identifier) = Just (Evaluatable.name identifier)

-- TODO: TypeIdentifier shouldn't evaluate to an address in the heap?
instance Evaluatable TypeIdentifier where
  eval _ _ TypeIdentifier{..} = do
    -- Add a reference to the type identifier in the current scope.
    span <- ask @Span
    reference (Reference (Evaluatable.name contents)) span ScopeGraph.TypeIdentifier (Declaration (Evaluatable.name contents))
    unit

data NestedTypeIdentifier a = NestedTypeIdentifier { left :: !a, right :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically NestedTypeIdentifier

instance Evaluatable NestedTypeIdentifier

data GenericType a = GenericType { genericTypeIdentifier :: !a, genericTypeArguments :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically GenericType

instance Evaluatable GenericType

data TypePredicate a = TypePredicate { typePredicateIdentifier :: !a, typePredicateType :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypePredicate

instance Evaluatable TypePredicate

newtype ObjectType a = ObjectType { objectTypeElements :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ObjectType

instance Evaluatable ObjectType

newtype ArrayType a = ArrayType { arrayType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ArrayType

instance Evaluatable ArrayType

newtype FlowMaybeType a = FlowMaybeType { flowMaybeType :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically FlowMaybeType

instance Evaluatable FlowMaybeType

newtype TypeQuery a = TypeQuery { typeQuerySubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeQuery

instance Evaluatable TypeQuery

newtype IndexTypeQuery a = IndexTypeQuery { indexTypeQuerySubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically IndexTypeQuery

instance Evaluatable IndexTypeQuery

newtype TypeArguments a = TypeArguments { typeArguments :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically TypeArguments

instance Evaluatable TypeArguments

newtype ExistentialType a = ExistentialType { contents :: T.Text }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically ExistentialType

instance Evaluatable ExistentialType

newtype LiteralType a = LiteralType { literalTypeSubject :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, NFData1, Ord, Show, ToJSONFields1, Traversable)
  deriving (Eq1, Show1, Ord1) via Generically LiteralType

instance Evaluatable LiteralType
