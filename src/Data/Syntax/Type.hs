{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, MultiParamTypeClasses, RecordWildCards, UndecidableInstances #-}
module Data.Syntax.Type (module Data.Syntax.Type) where

import Data.Abstract.Evaluatable
import Data.JSON.Fields
import Diffing.Algorithm
import Prelude hiding (Bool, Float, Int, Double)
import Prologue hiding (Map)

data Array a = Array { arraySize :: Maybe a, arrayElementType :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Array where liftEq = genericLiftEq
instance Ord1 Array where liftCompare = genericLiftCompare
instance Show1 Array where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Array
instance Evaluatable Array


-- TODO: What about type variables? re: FreeVariables1
data Annotation a = Annotation { annotationSubject :: a, annotationType :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

-- TODO: Specialize Evaluatable for Type to unify the inferred type of the subject with the specified type
instance Evaluatable Annotation where
  eval eval _ Annotation{..} = eval annotationSubject


data Function a = Function { functionParameters :: [a], functionReturn :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Function where liftEq = genericLiftEq
instance Ord1 Function where liftCompare = genericLiftCompare
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Function
instance Evaluatable Function


newtype Interface a = Interface { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Interface where liftEq = genericLiftEq
instance Ord1 Interface where liftCompare = genericLiftCompare
instance Show1 Interface where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Interface
instance Evaluatable Interface

data Map a = Map { mapKeyType :: a, mapElementType :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Map where liftEq = genericLiftEq
instance Ord1 Map where liftCompare = genericLiftCompare
instance Show1 Map where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Map
instance Evaluatable Map

newtype Parenthesized a = Parenthesized { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Parenthesized where liftEq = genericLiftEq
instance Ord1 Parenthesized where liftCompare = genericLiftCompare
instance Show1 Parenthesized where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Parenthesized
instance Evaluatable Parenthesized

newtype Pointer a = Pointer { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Pointer where liftEq = genericLiftEq
instance Ord1 Pointer where liftCompare = genericLiftCompare
instance Show1 Pointer where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Pointer
instance Evaluatable Pointer

newtype Product a = Product { values :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Product where liftEq = genericLiftEq
instance Ord1 Product where liftCompare = genericLiftCompare
instance Show1 Product where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Product
instance Evaluatable Product


data Readonly a = Readonly
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Readonly where liftEq = genericLiftEq
instance Ord1 Readonly where liftCompare = genericLiftCompare
instance Show1 Readonly where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Readonly
instance Evaluatable Readonly

newtype Slice a = Slice { value :: a }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Slice where liftEq = genericLiftEq
instance Ord1 Slice where liftCompare = genericLiftCompare
instance Show1 Slice where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Slice
instance Evaluatable Slice

newtype TypeParameters a = TypeParameters { terms :: [a] }
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 TypeParameters where liftEq = genericLiftEq
instance Ord1 TypeParameters where liftCompare = genericLiftCompare
instance Show1 TypeParameters where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeParameters
instance Evaluatable TypeParameters

-- data instead of newtype because no payload
data Void a = Void
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Void where liftEq = genericLiftEq
instance Ord1 Void where liftCompare = genericLiftCompare
instance Show1 Void where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Void
instance Evaluatable Void

-- data instead of newtype because no payload
data Int a = Int
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Int where liftEq = genericLiftEq
instance Ord1 Int where liftCompare = genericLiftCompare
instance Show1 Int where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Int
instance Evaluatable Int

data Float a = Float
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Float where liftEq = genericLiftEq
instance Ord1 Float where liftCompare = genericLiftCompare
instance Show1 Float where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Float
instance Evaluatable Float

data Double a = Double
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Double where liftEq = genericLiftEq
instance Ord1 Double where liftCompare = genericLiftCompare
instance Show1 Double where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Double
instance Evaluatable Double

data Bool a = Bool
  deriving (Declarations1, Diffable, Foldable, FreeVariables1, Functor, Generic1, Hashable1, ToJSONFields1, Traversable)

instance Eq1 Bool where liftEq = genericLiftEq
instance Ord1 Bool where liftCompare = genericLiftCompare
instance Show1 Bool where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Float
instance Evaluatable Bool
