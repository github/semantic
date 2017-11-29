{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, MultiParamTypeClasses #-}
module Data.Syntax.Type where

import Abstract.Eval
import Abstract.FreeVariables
import Algorithm
import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Ord.Generic
import Data.Functor.Classes.Show.Generic
import Data.Mergeable
import GHC.Generics

-- TODO: What about type variables? re: FreeVariables1
data Annotation a = Annotation { annotationSubject :: !a, annotationType :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1)

instance Eq1 Annotation where liftEq = genericLiftEq
instance Ord1 Annotation where liftCompare = genericLiftCompare
instance Show1 Annotation where liftShowsPrec = genericLiftShowsPrec

-- TODO: Specialize Eval for Type to unify the inferred type of the subject with the specified type
instance (Monad m) => Eval t v m Annotation where
  eval recur yield Annotation{..} = recur yield annotationSubject 


newtype Product a = Product { productElements :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Product where liftEq = genericLiftEq
instance Ord1 Product where liftCompare = genericLiftCompare
instance Show1 Product where liftShowsPrec = genericLiftShowsPrec

newtype TypeParameters a = TypeParameters { typeParameters :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 TypeParameters where liftEq = genericLiftEq
instance Ord1 TypeParameters where liftCompare = genericLiftCompare
instance Show1 TypeParameters where liftShowsPrec = genericLiftShowsPrec

data Readonly a = Readonly
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable)

instance Eq1 Readonly where liftEq = genericLiftEq
instance Ord1 Readonly where liftCompare = genericLiftCompare
instance Show1 Readonly where liftShowsPrec = genericLiftShowsPrec
