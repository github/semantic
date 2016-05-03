{-# LANGUAGE UndecidableInstances #-}
module Control.Comonad.CofreeExtras where

import Control.Comonad.Cofree
import Data.Copointed

unwrap :: Cofree functor annotation -> functor (Cofree functor annotation)
unwrap (_ :< f) = f

instance Copointed (Cofree functor) where
  copoint (annotation :< _) = annotation
