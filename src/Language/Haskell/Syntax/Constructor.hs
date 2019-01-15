{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Constructor where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

data UnitConstructor a = UnitConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically UnitConstructor

instance Evaluatable UnitConstructor

newtype TupleConstructor a = TupleConstructor { tupleConstructorArity :: Int }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TupleConstructor

instance Evaluatable TupleConstructor

data ListConstructor a = ListConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ListConstructor

instance Evaluatable ListConstructor

data FunctionConstructor a = FunctionConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically FunctionConstructor

instance Evaluatable FunctionConstructor

data RecordDataConstructor a = RecordDataConstructor { recordDataConstructorContext :: [a], recordDataConstructorName :: !a, recordDataConstructorFields :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically RecordDataConstructor

instance Evaluatable RecordDataConstructor

newtype TypeConstructorExport a = TypeConstructorExport { typeConstructorExportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TypeConstructorExport

instance Evaluatable TypeConstructorExport

data AllConstructors a = AllConstructors
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically AllConstructors

instance Evaluatable AllConstructors

newtype KindParenthesizedConstructor a = KindParenthesizedConstructor { kindParenthesizedConstructorContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically KindParenthesizedConstructor

instance Evaluatable KindParenthesizedConstructor

data GADTConstructor a = GADTConstructor { gadtConstructorContext :: a, gadtConstructorName :: a, gadtConstructorTypeSignature :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically GADTConstructor

instance Evaluatable GADTConstructor

newtype ConstructorSymbol a = ConstructorSymbol { constructorSymbolName :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ConstructorSymbol

instance Evaluatable ConstructorSymbol

data LabeledConstruction a = LabeledConstruction { labeledConstructionConstructor :: a, labeledConstructionFields :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LabeledConstruction

instance Evaluatable LabeledConstruction

data InfixDataConstructor a = InfixDataConstructor { infixDataConstructorContext :: [a], infixDataConstructorLeft :: a, infixDataConstructorOperator :: a, infixDataConstructorRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically InfixDataConstructor

instance Evaluatable InfixDataConstructor
