{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Type where

import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics
import Prologue hiding (Product)

data Annotation a = Annotation { annotationSubject :: !a, annotationType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

newtype Product a = Product { productElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Product where liftEq = genericLiftEq
instance Show1 Product where liftShowsPrec = genericLiftShowsPrec
