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
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Module

instance Evaluatable Module

data Type a = Type { typeIdentifier :: !a, typeParameters :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Type

instance Evaluatable Type

data TypeSynonym a = TypeSynonym { typeSynonymLeft :: !a, typeSynonymRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 TypeSynonym where liftEq = genericLiftEq
instance Ord1 TypeSynonym where liftCompare = genericLiftCompare
instance Show1 TypeSynonym where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 TypeSynonym

instance Evaluatable TypeSynonym

data UnitConstructor a = UnitConstructor a deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 UnitConstructor where liftEq = genericLiftEq
instance Ord1 UnitConstructor where liftCompare = genericLiftCompare
instance Show1 UnitConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 UnitConstructor

instance Evaluatable UnitConstructor

data TupleConstructor a = TupleConstructor a deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 TupleConstructor where liftEq = genericLiftEq
instance Ord1 TupleConstructor where liftCompare = genericLiftCompare
instance Show1 TupleConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 TupleConstructor

instance Evaluatable TupleConstructor

data ListConstructor a = ListConstructor a deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 ListConstructor where liftEq = genericLiftEq
instance Ord1 ListConstructor where liftCompare = genericLiftCompare
instance Show1 ListConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 ListConstructor

instance Evaluatable ListConstructor

data FunctionConstructor a = FunctionConstructor a deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 FunctionConstructor where liftEq = genericLiftEq
instance Ord1 FunctionConstructor where liftCompare = genericLiftCompare
instance Show1 FunctionConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 FunctionConstructor

instance Evaluatable FunctionConstructor
