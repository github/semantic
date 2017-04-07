{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, PolyKinds, TypeFamilies, TypeOperators #-}
module Data.Functor.Union where

import Data.Functor.Classes
import Data.Kind
import GHC.Show
import Prologue

-- | N-ary union of type constructors.
data Union (ts :: [k -> *]) (a :: k) where
  Here :: f a -> Union (f ': ts) a
  There :: Union ts a -> Union (f ': ts) a

-- | Embed a functor in a union and lift the union into a free monad.
wrapU :: (MonadFree (Union fs) m, InUnion fs f) => f (m a) -> m a
wrapU = wrap . emb

-- | Unwrap a cofree comonad and project a functor from the resulting union.
unwrapU :: (ComonadCofree (Union fs) w, InUnion fs f) => w a -> Maybe (f (w a))
unwrapU = proj . unwrap


-- Classes

class InUnion (fs :: [* -> *]) (f :: * -> *) where
  emb :: f a -> Union fs a
  proj :: Union fs a -> Maybe (f a)

type family Superset (combine :: [k] -> k -> Constraint) (fs :: [k]) (gs :: [k]) :: Constraint where
  Superset combine fs (g ': gs) = (combine fs g, Superset combine fs gs)
  Superset combine fs '[] = ()


-- Instances

instance {-# OVERLAPPABLE #-} InUnion (f ': fs) f where
  emb = Here
  proj (Here f) = Just f
  proj _ = Nothing

instance {-# OVERLAPPABLE #-} InUnion fs f => InUnion (g ': fs) f where
  emb f = There (emb f)
  proj (There fs) = proj fs
  proj _ = Nothing


instance (Foldable f, Foldable (Union fs)) => Foldable (Union (f ': fs)) where
  foldMap f (Here r) = foldMap f r
  foldMap f (There t) = foldMap f t

instance Foldable (Union '[]) where
  foldMap _ _ = mempty


instance (Eq (f a), Eq (Union fs a)) => Eq (Union (f ': fs) a) where
  Here f1 == Here f2 = f1 == f2
  There fs1 == There fs2 = fs1 == fs2
  _ == _ = False

instance Eq (Union '[] a) where
  _ == _ = True


instance (Show (f a), Show (Union fs a)) => Show (Union (f ': fs) a) where
  showsPrec d s = case s of
    Here f -> showsPrec d f
    There fs -> showsPrec d fs

instance Show (Union '[] a) where
  showsPrec _ _ = identity

instance (Eq1 f, Eq1 (Union fs)) => Eq1 (Union (f ': fs)) where
  liftEq eq (Here f) (Here g) = liftEq eq f g
  liftEq eq (There f) (There g) = liftEq eq f g
  liftEq _ _ _ = False

instance Eq1 (Union '[]) where
  liftEq _ _ _ = False -- We can never get here anyway.

instance (Show1 f, Show1 (Union fs)) => Show1 (Union (f ': fs)) where
  liftShowsPrec sp sl d (Here f) = showsUnaryWith (liftShowsPrec sp sl) "emb" d f
  liftShowsPrec sp sl d (There f) = liftShowsPrec sp sl d f

instance Show1 (Union '[]) where
  liftShowsPrec _ _ _ _ = identity
