{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, MultiParamTypeClasses #-}
module Data.Syntax.Type where

import Analysis.Abstract.Eval
import Data.Abstract.FreeVariables
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import Diffing.Algorithm
import GHC.Generics

data Array a = Array { arraySize :: Maybe a, arrayElementType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Array where liftEq = genericLiftEq
instance Ord1 Array where liftCompare = genericLiftCompare
instance Show1 Array where liftShowsPrec = genericLiftShowsPrec


-- TODO: What about type variables? re: FreeVariables1
data Annotation a = Annotation { annotationSubject :: !a, annotationType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

-- TODO: Specialize Eval for Type to unify the inferred type of the subject with the specified type
instance (Monad m) => Eval t v m Annotation where
  eval recur yield Annotation{..} = recur yield annotationSubject


data Function a = Function { functionParameters :: [a], functionReturn :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Function where liftEq = genericLiftEq
instance Ord1 Function where liftCompare = genericLiftCompare
instance Show1 Function where liftShowsPrec = genericLiftShowsPrec

newtype Interface a = Interface [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Interface where liftEq = genericLiftEq
instance Ord1 Interface where liftCompare = genericLiftCompare
instance Show1 Interface where liftShowsPrec = genericLiftShowsPrec

data Map a = Map { mapKeyType :: a, mapElementType :: a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Map where liftEq = genericLiftEq
instance Ord1 Map where liftCompare = genericLiftCompare
instance Show1 Map where liftShowsPrec = genericLiftShowsPrec

newtype Parenthesized a = Parenthesized a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Parenthesized where liftEq = genericLiftEq
instance Ord1 Parenthesized where liftCompare = genericLiftCompare
instance Show1 Parenthesized where liftShowsPrec = genericLiftShowsPrec

newtype Pointer a = Pointer a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Pointer where liftEq = genericLiftEq
instance Ord1 Pointer where liftCompare = genericLiftCompare
instance Show1 Pointer where liftShowsPrec = genericLiftShowsPrec

newtype Product a = Product [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Product where liftEq = genericLiftEq
instance Ord1 Product where liftCompare = genericLiftCompare
instance Show1 Product where liftShowsPrec = genericLiftShowsPrec

data Readonly a = Readonly
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Readonly where liftEq = genericLiftEq
instance Ord1 Readonly where liftCompare = genericLiftCompare
instance Show1 Readonly where liftShowsPrec = genericLiftShowsPrec

newtype Slice a = Slice a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Slice where liftEq = genericLiftEq
instance Ord1 Slice where liftCompare = genericLiftCompare
instance Show1 Slice where liftShowsPrec = genericLiftShowsPrec

data TypeParameters a = TypeParameters [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeParameters where liftEq = genericLiftEq
instance Ord1 TypeParameters where liftCompare = genericLiftCompare
instance Show1 TypeParameters where liftShowsPrec = genericLiftShowsPrec
