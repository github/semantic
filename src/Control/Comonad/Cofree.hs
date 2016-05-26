{-# LANGUAGE UndecidableInstances #-}
module Control.Comonad.Cofree where

import Prologue
import Data.Copointed

data Cofree functor annotation = annotation :< (functor (Cofree functor annotation))
  deriving (Functor, Foldable, Traversable)

instance (Eq annotation, Eq (functor (Cofree functor annotation))) => Eq (Cofree functor annotation) where
  a :< f == b :< g = a == b && f == g

instance (Show annotation, Show (functor (Cofree functor annotation))) => Show (Cofree functor annotation) where
  showsPrec n (a :< f) = showsPrec n a . (" :< " ++) . showsPrec n f

unwrap :: Cofree functor annotation -> functor (Cofree functor annotation)
unwrap (_ :< f) = f

unfold :: Functor functor => (seed -> (annotation, functor seed)) -> seed -> Cofree functor annotation
unfold grow seed = case grow seed of (annotation, functor) -> annotation :< (unfold grow <$> functor)

instance Copointed (Cofree functor) where
  copoint (annotation :< _) = annotation
