{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Type where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

data StrictType a = StrictType { strictTypeIdentifier :: a, strictTypeParameters :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 StrictType where liftEq = genericLiftEq
instance Ord1 StrictType where liftCompare = genericLiftCompare
instance Show1 StrictType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictType

newtype StrictTypeVariable a = StrictTypeVariable { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 StrictTypeVariable where liftEq = genericLiftEq
instance Ord1 StrictTypeVariable where liftCompare = genericLiftCompare
instance Show1 StrictTypeVariable where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StrictTypeVariable

data Type a = Type { typeIdentifier :: a, typeParameters :: a, typeKindSignature :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Type

data TypeSynonym a = TypeSynonym { typeSynonymLeft :: a, typeSynonymContext :: [a], typeSynonymRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeSynonym where liftEq = genericLiftEq
instance Ord1 TypeSynonym where liftCompare = genericLiftCompare
instance Show1 TypeSynonym where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeSynonym

data AnnotatedTypeVariable a = AnnotatedTypeVariable { annotatedTypeVariableIdentifier :: a, annotatedTypeVariableannotation :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 AnnotatedTypeVariable where liftEq = genericLiftEq
instance Ord1 AnnotatedTypeVariable where liftCompare = genericLiftCompare
instance Show1 AnnotatedTypeVariable where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AnnotatedTypeVariable

data StandaloneDerivingInstance a = StandaloneDerivingInstance { standaloneDerivingInstanceContext :: [a], standaloneDerivingInstanceClass :: a, standaloneDerivingInstanceInstance :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 StandaloneDerivingInstance where liftEq = genericLiftEq
instance Ord1 StandaloneDerivingInstance where liftCompare = genericLiftCompare
instance Show1 StandaloneDerivingInstance where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable StandaloneDerivingInstance

data FunctionType a = FunctionType { functionTypeLeft :: a, functionTypeRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FunctionType where liftEq = genericLiftEq
instance Ord1 FunctionType where liftCompare = genericLiftCompare
instance Show1 FunctionType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionType

data TypeSignature a = TypeSignature { typeSignatureName :: [a], typeSignatureContext :: [a], typeSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeSignature where liftEq = genericLiftEq
instance Ord1 TypeSignature where liftCompare = genericLiftCompare
instance Show1 TypeSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeSignature

data ExpressionTypeSignature a = ExpressionTypeSignature { expressionTypeSignatureName :: [a], expressionTypeSignatureContext :: [a], expressionTypeSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ExpressionTypeSignature where liftEq = genericLiftEq
instance Ord1 ExpressionTypeSignature where liftCompare = genericLiftCompare
instance Show1 ExpressionTypeSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ExpressionTypeSignature

newtype KindSignature a = KindSignature { kindSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 KindSignature where liftEq = genericLiftEq
instance Ord1 KindSignature where liftCompare = genericLiftCompare
instance Show1 KindSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindSignature

data KindFunctionType a = KindFunctionType { kindFunctionTypeLeft :: a, kindFunctionTypeRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 KindFunctionType where liftEq = genericLiftEq
instance Ord1 KindFunctionType where liftCompare = genericLiftCompare
instance Show1 KindFunctionType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindFunctionType

newtype Kind a = Kind { kindKind :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Kind where liftEq = genericLiftEq
instance Ord1 Kind where liftCompare = genericLiftCompare
instance Show1 Kind where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Kind

newtype KindListType a = KindListType { kindListTypeKind :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 KindListType where liftEq = genericLiftEq
instance Ord1 KindListType where liftCompare = genericLiftCompare
instance Show1 KindListType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindListType

data Star a = Star
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Star where liftEq = genericLiftEq
instance Ord1 Star where liftCompare = genericLiftCompare
instance Show1 Star where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Star

data EqualityConstraint a = EqualityConstraint { equalityConstraintLeft :: a, equalityConstraintRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 EqualityConstraint where liftEq = genericLiftEq
instance Ord1 EqualityConstraint where liftCompare = genericLiftCompare
instance Show1 EqualityConstraint where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable EqualityConstraint

-- e.g. `type instance F [Int] = Int` where `F` is an open type family.
data TypeInstance a = TypeInstance { typeInstanceType :: a, typeInstanceBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeInstance where liftEq = genericLiftEq
instance Ord1 TypeInstance where liftCompare = genericLiftCompare
instance Show1 TypeInstance where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeInstance

data TypeClassInstance a = TypeClassInstance { typeClassInstanceContext :: [a], typeClassInstanceIdentifier :: a, typeClassInstanceInstance :: a, typeClassInstanceBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeClassInstance where liftEq = genericLiftEq
instance Ord1 TypeClassInstance where liftCompare = genericLiftCompare
instance Show1 TypeClassInstance where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeClassInstance

newtype Instance a = Instance { value :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 Instance where liftEq = genericLiftEq
instance Ord1 Instance where liftCompare = genericLiftCompare
instance Show1 Instance where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Instance

newtype KindTupleType a = KindTupleType { kindTupleType :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 KindTupleType where liftEq = genericLiftEq
instance Ord1 KindTupleType where liftCompare = genericLiftCompare
instance Show1 KindTupleType where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindTupleType

data TypeClass a = TypeClass { typeClassContext :: a, typeClassIdentifier :: a, typeClassParameters :: [a], typeClassBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeClass where liftEq = genericLiftEq
instance Ord1 TypeClass where liftCompare = genericLiftCompare
instance Show1 TypeClass where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeClass

-- The default signature of a type class. The default signature has the same shape as a TypeSignature Assignment.
data DefaultSignature a = DefaultSignature { defaultSignatureName :: [a], defaultSignatureContext :: [a], defaultSignatureContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 DefaultSignature where liftEq = genericLiftEq
instance Ord1 DefaultSignature where liftCompare = genericLiftCompare
instance Show1 DefaultSignature where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable DefaultSignature

data TypeFamily a = TypeFamily { typeFamilyIdentifier :: a, typeFamilyParameters :: [a], typeFamilySignature :: a, typeFamilyBody :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeFamily where liftEq = genericLiftEq
instance Ord1 TypeFamily where liftCompare = genericLiftCompare
instance Show1 TypeFamily where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeFamily

newtype FunctionalDependency a = FunctionalDependency { functionalDependencyContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FunctionalDependency where liftEq = genericLiftEq
instance Ord1 FunctionalDependency where liftCompare = genericLiftCompare
instance Show1 FunctionalDependency where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionalDependency
