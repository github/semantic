{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Type where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

data StrictType a = StrictType { strictTypeIdentifier :: a, strictTypeParameters :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically StrictType

instance Evaluatable StrictType

newtype StrictTypeVariable a = StrictTypeVariable { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically StrictTypeVariable

instance Evaluatable StrictTypeVariable

data Type a = Type { typeIdentifier :: a, typeParameters :: a, typeKindSignature :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Type

instance Evaluatable Type

data TypeSynonym a = TypeSynonym { typeSynonymLeft :: a, typeSynonymContext :: [a], typeSynonymRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeSynonym

instance Evaluatable TypeSynonym

data AnnotatedTypeVariable a = AnnotatedTypeVariable { annotatedTypeVariableIdentifier :: a, annotatedTypeVariableannotation :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically AnnotatedTypeVariable

instance Evaluatable AnnotatedTypeVariable

data StandaloneDerivingInstance a = StandaloneDerivingInstance { standaloneDerivingInstanceContext :: [a], standaloneDerivingInstanceClass :: a, standaloneDerivingInstanceInstance :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically StandaloneDerivingInstance

instance Evaluatable StandaloneDerivingInstance

data FunctionType a = FunctionType { functionTypeLeft :: a, functionTypeRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FunctionType

instance Evaluatable FunctionType

data TypeSignature a = TypeSignature { typeSignatureName :: [a], typeSignatureContext :: [a], typeSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeSignature

instance Evaluatable TypeSignature

data ExpressionTypeSignature a = ExpressionTypeSignature { expressionTypeSignatureName :: [a], expressionTypeSignatureContext :: [a], expressionTypeSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ExpressionTypeSignature

instance Evaluatable ExpressionTypeSignature

newtype KindSignature a = KindSignature { kindSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically KindSignature

instance Evaluatable KindSignature

data KindFunctionType a = KindFunctionType { kindFunctionTypeLeft :: a, kindFunctionTypeRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically KindFunctionType

instance Evaluatable KindFunctionType

newtype Kind a = Kind { kindKind :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Kind

instance Evaluatable Kind

newtype KindListType a = KindListType { kindListTypeKind :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically KindListType

instance Evaluatable KindListType

data Star a = Star
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Star

instance Evaluatable Star

data EqualityConstraint a = EqualityConstraint { equalityConstraintLeft :: a, equalityConstraintRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically EqualityConstraint

instance Evaluatable EqualityConstraint

-- e.g. `type instance F [Int] = Int` where `F` is an open type family.
data TypeInstance a = TypeInstance { typeInstanceType :: a, typeInstanceBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeInstance

instance Evaluatable TypeInstance

data TypeClassInstance a = TypeClassInstance { typeClassInstanceContext :: [a], typeClassInstanceIdentifier :: a, typeClassInstanceInstance :: a, typeClassInstanceBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeClassInstance

instance Evaluatable TypeClassInstance

newtype Instance a = Instance { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Instance

instance Evaluatable Instance

newtype KindTupleType a = KindTupleType { kindTupleType :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically KindTupleType

instance Evaluatable KindTupleType

data TypeClass a = TypeClass { typeClassContext :: a, typeClassIdentifier :: a, typeClassParameters :: [a], typeClassBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeClass

instance Evaluatable TypeClass

-- The default signature of a type class. The default signature has the same shape as a TypeSignature Assignment.
data DefaultSignature a = DefaultSignature { defaultSignatureName :: [a], defaultSignatureContext :: [a], defaultSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically DefaultSignature

instance Evaluatable DefaultSignature

data TypeFamily a = TypeFamily { typeFamilyIdentifier :: a, typeFamilyParameters :: [a], typeFamilySignature :: a, typeFamilyBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeFamily

instance Evaluatable TypeFamily

newtype FunctionalDependency a = FunctionalDependency { functionalDependencyContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FunctionalDependency

instance Evaluatable FunctionalDependency
