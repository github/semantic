{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Type where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics

data Annotation a = Annotation { annotationSubject :: !a, annotationType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

newtype Product a = Product { productElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Product where liftEq = genericLiftEq
instance Show1 Product where liftShowsPrec = genericLiftShowsPrec

data Visibility a = Visibility { visibilitySubject :: !a, visibilityType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Visibility where liftEq = genericLiftEq
instance Show1 Visibility where liftShowsPrec = genericLiftShowsPrec

data TypeParameters a = TypeParameters { typeParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeParameters where liftEq = genericLiftEq
instance Show1 TypeParameters where liftShowsPrec = genericLiftShowsPrec

data Readonly a = Readonly a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Readonly where liftEq = genericLiftEq
instance Show1 Readonly where liftShowsPrec = genericLiftShowsPrec
