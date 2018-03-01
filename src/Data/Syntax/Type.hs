{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, MultiParamTypeClasses, UndecidableInstances #-}
module Data.Syntax.Type where

import Data.Abstract.Evaluatable
import Data.Abstract.FreeVariables
import Diffing.Algorithm
import Prologue hiding (Map)

data Array a = Array { arraySize :: Maybe a, arrayElementType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Array where liftEq = genericLiftEq
instance Ord1 Array where liftCompare = genericLiftCompare
instance Show1 Array where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Array
instance Evaluatable Array


-- TODO: What about type variables? re: FreeVariables1
data Annotation a = Annotation { annotationSubject :: !a, annotationType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

-- TODO: Specialize Evaluatable for Type to unify the inferred type of the subject with the specified type
instance Evaluatable Annotation where
  eval Annotation{annotationSubject = Subterm _ action} = action


data Function a = Function { functionParameters :: [a], functionReturn :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Function where liftEq = genericLiftEq
instance Ord1 Function where liftCompare = genericLiftCompare
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Function
instance Evaluatable Function


newtype Interface a = Interface [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Interface where liftEq = genericLiftEq
instance Ord1 Interface where liftCompare = genericLiftCompare
instance Show1 Interface where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Interface
instance Evaluatable Interface


data Map a = Map { mapKeyType :: a, mapElementType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Map where liftEq = genericLiftEq
instance Ord1 Map where liftCompare = genericLiftCompare
instance Show1 Map where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Map
instance Evaluatable Map


newtype Parenthesized a = Parenthesized a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Parenthesized where liftEq = genericLiftEq
instance Ord1 Parenthesized where liftCompare = genericLiftCompare
instance Show1 Parenthesized where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Parenthesized
instance Evaluatable Parenthesized


newtype Pointer a = Pointer a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Pointer where liftEq = genericLiftEq
instance Ord1 Pointer where liftCompare = genericLiftCompare
instance Show1 Pointer where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Pointer
instance Evaluatable Pointer


newtype Product a = Product [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Product where liftEq = genericLiftEq
instance Ord1 Product where liftCompare = genericLiftCompare
instance Show1 Product where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Product
instance Evaluatable Product


data Readonly a = Readonly
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Readonly where liftEq = genericLiftEq
instance Ord1 Readonly where liftCompare = genericLiftCompare
instance Show1 Readonly where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Readonly
instance Evaluatable Readonly


newtype Slice a = Slice a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Slice where liftEq = genericLiftEq
instance Ord1 Slice where liftCompare = genericLiftCompare
instance Show1 Slice where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for Slice
instance Evaluatable Slice


newtype TypeParameters a = TypeParameters [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 TypeParameters where liftEq = genericLiftEq
instance Ord1 TypeParameters where liftCompare = genericLiftCompare
instance Show1 TypeParameters where liftShowsPrec = genericLiftShowsPrec

-- TODO: Implement Eval instance for TypeParameters
instance Evaluatable TypeParameters
