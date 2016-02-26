{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Free where

data Free functor pure = Free (functor (Free functor pure)) | Pure pure
  deriving (Functor, Foldable, Traversable)

instance (Eq pure, Eq (functor (Free functor pure))) => Eq (Free functor pure) where
  Pure a == Pure b = a == b
  Free f == Free g = f == g
  _ == _ = False

iter :: Functor functor => (functor pure -> pure) -> Free functor pure -> pure
iter _ (Pure a) = a
iter f (Free g) = f (iter f <$> g)
