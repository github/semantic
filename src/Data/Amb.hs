{-# LANGUAGE MultiParamTypeClasses, NoStrictData #-}
module Data.Amb where

import Control.Monad.Error.Class
import Data.Bifunctor
import Data.List.NonEmpty
import Data.Semigroup

data Amb l r
  = None l
  | Some (NonEmpty r)
  deriving (Eq, Foldable, Functor, Show, Traversable)

disamb :: (l -> a) -> (NonEmpty r -> a) -> Amb l r -> a
disamb f _ (None l) = f l
disamb _ g (Some r) = g r


instance Bifunctor Amb where
  bimap f _ (None l) = None (f l)
  bimap _ g (Some r) = Some (fmap g r)

instance Semigroup (Amb l r) where
  l <> None _ = l
  None _ <> r = r
  Some as <> Some bs = Some (as <> bs)

instance Applicative (Amb l) where
  pure a = Some (a :| [])
  None l <*> _ = None l
  _ <*> None r = None r
  Some fs <*> Some as = Some (fs <*> as)

instance Monad (Amb l) where
  return = pure
  None a >>= _ = None a
  Some as >>= f = foldr1 (<>) (f <$> as)

instance MonadError l (Amb l) where
  throwError = None
  None a  `catchError` f = f a
  Some as `catchError` _ = Some as
