{-# LANGUAGE DeriveAnyClass #-}
module Language.Haskell.Syntax where

import           Data.Abstract.Evaluatable
import           Data.JSON.Fields
import           Diffing.Algorithm
import           Prelude
import           Prologue

data Module a = Module { moduleIdentifier :: !a
                       , moduleExports    :: ![a]
                       , moduleStatements :: !a
                       }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module

data Type a = Type { typeIdentifier :: !a, typeParameters :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Type

data TypeSynonym a = TypeSynonym { typeSynonymLeft :: !a, typeSynonymRight :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 TypeSynonym where liftEq = genericLiftEq
instance Ord1 TypeSynonym where liftCompare = genericLiftCompare
instance Show1 TypeSynonym where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TypeSynonym

data UnitConstructor a = UnitConstructor deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 UnitConstructor where liftEq = genericLiftEq
instance Ord1 UnitConstructor where liftCompare = genericLiftCompare
instance Show1 UnitConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable UnitConstructor

newtype TupleConstructor a = TupleConstructor { tupleConstructorArity :: Int } deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 TupleConstructor where liftEq = genericLiftEq
instance Ord1 TupleConstructor where liftCompare = genericLiftCompare
instance Show1 TupleConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable TupleConstructor

data ListConstructor a = ListConstructor deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 ListConstructor where liftEq = genericLiftEq
instance Ord1 ListConstructor where liftCompare = genericLiftCompare
instance Show1 ListConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable ListConstructor

data FunctionConstructor a = FunctionConstructor deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, Mergeable, FreeVariables1, Declarations1, ToJSONFields1)

instance Eq1 FunctionConstructor where liftEq = genericLiftEq
instance Ord1 FunctionConstructor where liftCompare = genericLiftCompare
instance Show1 FunctionConstructor where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable FunctionConstructor
