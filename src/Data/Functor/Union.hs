{-# LANGUAGE DataKinds, GADTs, KindSignatures, MultiParamTypeClasses, TypeOperators #-}
module Data.Functor.Union where

import GHC.Show
import Prologue

-- | N-ary union of type constructors.
data Union (ts :: [* -> *]) (a :: *) where
  Here :: f a -> Union (f ': ts) a
  There :: Union ts a -> Union (f ': ts) a


-- Classes

class InUnion (fs :: [* -> *]) (f :: * -> *) where
  emb :: f a -> Union fs a
  proj :: Union fs a -> Maybe (f a)


-- Instances

instance {-# OVERLAPPABLE #-} InUnion (f ': fs) f where
  emb = Here
  proj (Here f) = Just f
  proj _ = Nothing

instance {-# OVERLAPPABLE #-} InUnion fs f => InUnion (g ': fs) f where
  emb f = There (emb f)
  proj (There fs) = proj fs
  proj _ = Nothing


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
