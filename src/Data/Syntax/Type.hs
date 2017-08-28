{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Type where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Pretty.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics

data Annotation a = Annotation { annotationSubject :: !a, annotationType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Annotation where liftPretty = genericLiftPretty

newtype Product a = Product { productElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Product where liftEq = genericLiftEq
instance Show1 Product where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Product where liftPretty = genericLiftPretty

data Visibility a = Visibility { visibilitySubject :: !a, visibilityType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Visibility where liftEq = genericLiftEq
instance Show1 Visibility where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Visibility where liftPretty = genericLiftPretty

data TypeParameters a = TypeParameters { typeParameters :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TypeParameters where liftEq = genericLiftEq
instance Show1 TypeParameters where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 TypeParameters where liftPretty = genericLiftPretty

data Readonly a = Readonly
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Readonly where liftEq = genericLiftEq
instance Show1 Readonly where liftShowsPrec = genericLiftShowsPrec
