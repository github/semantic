{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.Syntax.Constructor where

import Prologue

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Proto3.Suite.Class

data UnitConstructor a = UnitConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 UnitConstructor where liftEq = genericLiftEq
instance Ord1 UnitConstructor where liftCompare = genericLiftCompare
instance Show1 UnitConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable UnitConstructor

newtype TupleConstructor a = TupleConstructor { tupleConstructorArity :: Int }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TupleConstructor where liftEq = genericLiftEq
instance Ord1 TupleConstructor where liftCompare = genericLiftCompare
instance Show1 TupleConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TupleConstructor

data ListConstructor a = ListConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ListConstructor where liftEq = genericLiftEq
instance Ord1 ListConstructor where liftCompare = genericLiftCompare
instance Show1 ListConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ListConstructor

data FunctionConstructor a = FunctionConstructor
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 FunctionConstructor where liftEq = genericLiftEq
instance Ord1 FunctionConstructor where liftCompare = genericLiftCompare
instance Show1 FunctionConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionConstructor

data RecordDataConstructor a = RecordDataConstructor { recordDataConstructorContext :: [a], recordDataConstructorName :: !a, recordDataConstructorFields :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 RecordDataConstructor where liftEq = genericLiftEq
instance Ord1 RecordDataConstructor where liftCompare = genericLiftCompare
instance Show1 RecordDataConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable RecordDataConstructor

newtype TypeConstructorExport a = TypeConstructorExport { typeConstructorExportContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 TypeConstructorExport where liftEq = genericLiftEq
instance Ord1 TypeConstructorExport where liftCompare = genericLiftCompare
instance Show1 TypeConstructorExport where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeConstructorExport

data AllConstructors a = AllConstructors
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 AllConstructors where liftEq = genericLiftEq
instance Ord1 AllConstructors where liftCompare = genericLiftCompare
instance Show1 AllConstructors where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable AllConstructors
newtype KindParenthesizedConstructor a = KindParenthesizedConstructor { kindParenthesizedConstructorContent :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 KindParenthesizedConstructor where liftEq = genericLiftEq
instance Ord1 KindParenthesizedConstructor where liftCompare = genericLiftCompare
instance Show1 KindParenthesizedConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable KindParenthesizedConstructor

data GADTConstructor a = GADTConstructor { gadtConstructorContext :: a, gadtConstructorName :: a, gadtConstructorTypeSignature :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 GADTConstructor where liftEq = genericLiftEq
instance Ord1 GADTConstructor where liftCompare = genericLiftCompare
instance Show1 GADTConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable GADTConstructor

newtype ConstructorSymbol a = ConstructorSymbol { constructorSymbolName :: Name }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 ConstructorSymbol where liftEq = genericLiftEq
instance Ord1 ConstructorSymbol where liftCompare = genericLiftCompare
instance Show1 ConstructorSymbol where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ConstructorSymbol

data LabeledConstruction a = LabeledConstruction { labeledConstructionConstructor :: a, labeledConstructionFields :: [a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 LabeledConstruction where liftEq = genericLiftEq
instance Ord1 LabeledConstruction where liftCompare = genericLiftCompare
instance Show1 LabeledConstruction where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable LabeledConstruction

data InfixDataConstructor a = InfixDataConstructor { infixDataConstructorContext :: [a], infixDataConstructorLeft :: a, infixDataConstructorOperator :: a, infixDataConstructorRight :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Ord, Show, ToJSONFields1, Traversable, Named1, Message1, NFData1)

instance Eq1 InfixDataConstructor where liftEq = genericLiftEq
instance Ord1 InfixDataConstructor where liftCompare = genericLiftCompare
instance Show1 InfixDataConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable InfixDataConstructor
