{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Identifier where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Tags.Taggable (Taggable)

newtype QualifiedTypeClassIdentifier a = QualifiedTypeClassIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedTypeClassIdentifier

instance Hashable1 QualifiedTypeClassIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedTypeClassIdentifier

newtype QualifiedTypeConstructorIdentifier a = QualifiedTypeConstructorIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedTypeConstructorIdentifier

instance Hashable1 QualifiedTypeConstructorIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedTypeConstructorIdentifier

newtype QualifiedConstructorIdentifier a = QualifiedConstructorIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedConstructorIdentifier

instance Hashable1 QualifiedConstructorIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedConstructorIdentifier

newtype QualifiedInfixVariableIdentifier a = QualifiedInfixVariableIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedInfixVariableIdentifier

instance Hashable1 QualifiedInfixVariableIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedInfixVariableIdentifier

newtype QualifiedModuleIdentifier a = QualifiedModuleIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedModuleIdentifier

instance Hashable1 QualifiedModuleIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedModuleIdentifier

newtype QualifiedVariableIdentifier a = QualifiedVariableIdentifier { values :: NonEmpty a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically QualifiedVariableIdentifier

instance Hashable1 QualifiedVariableIdentifier where liftHashWithSalt = foldl

instance Evaluatable QualifiedVariableIdentifier

newtype TypeVariableIdentifier a = TypeVariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeVariableIdentifier

instance Evaluatable TypeVariableIdentifier

newtype TypeConstructorIdentifier a = TypeConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeConstructorIdentifier

instance Evaluatable TypeConstructorIdentifier

newtype ModuleIdentifier a = ModuleIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ModuleIdentifier

instance Evaluatable ModuleIdentifier

newtype ConstructorIdentifier a = ConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ConstructorIdentifier

instance Evaluatable ConstructorIdentifier

newtype ImplicitParameterIdentifier a = ImplicitParameterIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ImplicitParameterIdentifier

instance Evaluatable ImplicitParameterIdentifier

newtype InfixConstructorIdentifier a = InfixConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InfixConstructorIdentifier

instance Evaluatable InfixConstructorIdentifier

newtype InfixVariableIdentifier a = InfixVariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InfixVariableIdentifier

instance Evaluatable InfixVariableIdentifier

newtype TypeClassIdentifier a = TypeClassIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeClassIdentifier

instance Evaluatable TypeClassIdentifier

newtype VariableIdentifier a = VariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically VariableIdentifier

instance Evaluatable VariableIdentifier

newtype PrimitiveConstructorIdentifier a = PrimitiveConstructorIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PrimitiveConstructorIdentifier

instance Evaluatable PrimitiveConstructorIdentifier

newtype PrimitiveVariableIdentifier a = PrimitiveVariableIdentifier { name :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, Taggable, ToJSONFields1, Traversable, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically PrimitiveVariableIdentifier

instance Evaluatable PrimitiveVariableIdentifier
