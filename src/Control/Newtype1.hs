{-# LANGUAGE DefaultSignatures, TypeFamilies, UndecidableInstances #-}
module Control.Newtype1
( Newtype1(..)
) where

import Data.Coerce (coerce)
import Prologue

class Newtype1 n where
  type O1 n :: * -> *
  type O1 n = GO1 (Rep1 n)

  pack1 :: O1 n a -> n a
  default pack1 :: (Generic1 n, GNewtype1 (Rep1 n), O1 n ~ GO1 (Rep1 n)) => O1 n a -> n a
  pack1 = to1 . gpack1

  unpack1 :: n a -> O1 n a
  default unpack1 :: (Generic1 n, GNewtype1 (Rep1 n), O1 n ~ GO1 (Rep1 n)) => n a -> O1 n a
  unpack1 = gunpack1 . from1

class GNewtype1 n where
  type GO1 n :: * -> *

  gpack1 :: GO1 n a -> n a
  gunpack1 :: n a -> GO1 n a

instance GNewtype1 (D1 d (C1 c (S1 s (Rec1 a)))) where
  type GO1 (D1 d (C1 c (S1 s (Rec1 a)))) = a
  gpack1 = coerce
  gunpack1 = coerce
