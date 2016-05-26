{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Free where

import Prologue

data Free functor pure = Free (functor (Free functor pure)) | Pure pure
  deriving (Functor, Foldable, Traversable)

instance (Eq pure, Eq (functor (Free functor pure))) => Eq (Free functor pure) where
  Pure a == Pure b = a == b
  Free f == Free g = f == g
  _ == _ = False

instance (Show pure, Show (functor (Free functor pure))) => Show (Free functor pure) where
  showsPrec n (Pure a) = ("Pure " ++) . showsPrec n a
  showsPrec n (Free f) = ("Free " ++) . showsPrec n f

iter :: Functor functor => (functor pure -> pure) -> Free functor pure -> pure
iter _ (Pure a) = a
iter f (Free g) = f (iter f <$> g)
