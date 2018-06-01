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
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Module

instance Evaluatable Module

data StrictType a = StrictType { strictTypeIdentifier :: !a, strictTypeParameters :: !a }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 StrictType where liftEq = genericLiftEq
instance Ord1 StrictType where liftCompare = genericLiftCompare
instance Show1 StrictType where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 StrictType

instance Evaluatable StrictType

newtype StrictTypeVariable a = StrictTypeVariable { strictTypeVariableIdentifier :: a }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 StrictTypeVariable where liftEq = genericLiftEq
instance Ord1 StrictTypeVariable where liftCompare = genericLiftCompare
instance Show1 StrictTypeVariable where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 StrictTypeVariable

instance Evaluatable StrictTypeVariable

data Type a = Type { typeIdentifier :: !a, typeParameters :: !a }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Type where liftEq = genericLiftEq
instance Ord1 Type where liftCompare = genericLiftCompare
instance Show1 Type where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Type

instance Evaluatable Type

data TypeSynonym a = TypeSynonym { typeSynonymLeft :: !a, typeSynonymRight :: !a }
  deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 TypeSynonym where liftEq = genericLiftEq
instance Ord1 TypeSynonym where liftCompare = genericLiftCompare
instance Show1 TypeSynonym where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 TypeSynonym

instance Evaluatable TypeSynonym

data UnitConstructor a = UnitConstructor deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 UnitConstructor where liftEq = genericLiftEq
instance Ord1 UnitConstructor where liftCompare = genericLiftCompare
instance Show1 UnitConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 UnitConstructor

instance Evaluatable UnitConstructor

newtype TupleConstructor a = TupleConstructor { tupleConstructorArity :: Int } deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 TupleConstructor where liftEq = genericLiftEq
instance Ord1 TupleConstructor where liftCompare = genericLiftCompare
instance Show1 TupleConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 TupleConstructor

instance Evaluatable TupleConstructor

data ListConstructor a = ListConstructor deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 ListConstructor where liftEq = genericLiftEq
instance Ord1 ListConstructor where liftCompare = genericLiftCompare
instance Show1 ListConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 ListConstructor

instance Evaluatable ListConstructor

data FunctionConstructor a = FunctionConstructor deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 FunctionConstructor where liftEq = genericLiftEq
instance Ord1 FunctionConstructor where liftCompare = genericLiftCompare
instance Show1 FunctionConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 FunctionConstructor

instance Evaluatable FunctionConstructor

data RecordDataConstructor a = RecordDataConstructor { recordDataConstructorName :: !a, recordDataConstructorFields :: !a } deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 RecordDataConstructor where liftEq = genericLiftEq
instance Ord1 RecordDataConstructor where liftCompare = genericLiftCompare
instance Show1 RecordDataConstructor where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 RecordDataConstructor

instance Evaluatable RecordDataConstructor

data Field a = Field { fieldName :: !a, fieldBody :: !a } deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Field where liftEq = genericLiftEq
instance Ord1 Field where liftCompare = genericLiftCompare
instance Show1 Field where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Field

instance Evaluatable Field

newtype Pragma a = Pragma ByteString deriving (Diffable, Eq, Foldable, Functor, Generic1, Hashable1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Pragma where liftEq = genericLiftEq
instance Ord1 Pragma where liftCompare = genericLiftCompare
instance Show1 Pragma where liftShowsPrec = genericLiftShowsPrec

instance ToJSONFields1 Pragma

instance Evaluatable Pragma
