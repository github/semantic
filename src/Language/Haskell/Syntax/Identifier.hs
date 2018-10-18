{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Identifier where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

newtype QualifiedTypeClassIdentifier a = QualifiedTypeClassIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QualifiedTypeClassIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedTypeClassIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedTypeClassIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedTypeClassIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedTypeClassIdentifier

newtype QualifiedTypeConstructorIdentifier a = QualifiedTypeConstructorIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QualifiedTypeConstructorIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedTypeConstructorIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedTypeConstructorIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedTypeConstructorIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedTypeConstructorIdentifier

newtype QualifiedConstructorIdentifier a = QualifiedConstructorIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QualifiedConstructorIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedConstructorIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedConstructorIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedConstructorIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedConstructorIdentifier

newtype QualifiedInfixVariableIdentifier a = QualifiedInfixVariableIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QualifiedInfixVariableIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedInfixVariableIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedInfixVariableIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedInfixVariableIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedInfixVariableIdentifier

newtype QualifiedModuleIdentifier a = QualifiedModuleIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QualifiedModuleIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedModuleIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedModuleIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedModuleIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedModuleIdentifier

newtype QualifiedVariableIdentifier a = QualifiedVariableIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 QualifiedVariableIdentifier where liftEq = genericLiftEq
instance Ord1 QualifiedVariableIdentifier where liftCompare = genericLiftCompare
instance Show1 QualifiedVariableIdentifier where liftShowsPrec = genericLiftShowsPrec
instance Hashable1 QualifiedVariableIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedVariableIdentifier

newtype TypeVariableIdentifier a = TypeVariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeVariableIdentifier where liftEq = genericLiftEq
instance Ord1 TypeVariableIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeVariableIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeVariableIdentifier

newtype TypeConstructorIdentifier a = TypeConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeConstructorIdentifier where liftEq = genericLiftEq
instance Ord1 TypeConstructorIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeConstructorIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeConstructorIdentifier

newtype ModuleIdentifier a = ModuleIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ModuleIdentifier where liftEq = genericLiftEq
instance Ord1 ModuleIdentifier where liftCompare = genericLiftCompare
instance Show1 ModuleIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ModuleIdentifier

newtype ConstructorIdentifier a = ConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ConstructorIdentifier where liftEq = genericLiftEq
instance Ord1 ConstructorIdentifier where liftCompare = genericLiftCompare
instance Show1 ConstructorIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructorIdentifier

newtype ImplicitParameterIdentifier a = ImplicitParameterIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ImplicitParameterIdentifier where liftEq = genericLiftEq
instance Ord1 ImplicitParameterIdentifier where liftCompare = genericLiftCompare
instance Show1 ImplicitParameterIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ImplicitParameterIdentifier

newtype InfixConstructorIdentifier a = InfixConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 InfixConstructorIdentifier where liftEq = genericLiftEq
instance Ord1 InfixConstructorIdentifier where liftCompare = genericLiftCompare
instance Show1 InfixConstructorIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InfixConstructorIdentifier

newtype InfixVariableIdentifier a = InfixVariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 InfixVariableIdentifier where liftEq = genericLiftEq
instance Ord1 InfixVariableIdentifier where liftCompare = genericLiftCompare
instance Show1 InfixVariableIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InfixVariableIdentifier

newtype TypeClassIdentifier a = TypeClassIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeClassIdentifier where liftEq = genericLiftEq
instance Ord1 TypeClassIdentifier where liftCompare = genericLiftCompare
instance Show1 TypeClassIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeClassIdentifier

newtype VariableIdentifier a = VariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 VariableIdentifier where liftEq = genericLiftEq
instance Ord1 VariableIdentifier where liftCompare = genericLiftCompare
instance Show1 VariableIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable VariableIdentifier

newtype PrimitiveConstructorIdentifier a = PrimitiveConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 PrimitiveConstructorIdentifier where liftEq = genericLiftEq
instance Ord1 PrimitiveConstructorIdentifier where liftCompare = genericLiftCompare
instance Show1 PrimitiveConstructorIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PrimitiveConstructorIdentifier

newtype PrimitiveVariableIdentifier a = PrimitiveVariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 PrimitiveVariableIdentifier where liftEq = genericLiftEq
instance Ord1 PrimitiveVariableIdentifier where liftCompare = genericLiftCompare
instance Show1 PrimitiveVariableIdentifier where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable PrimitiveVariableIdentifier
