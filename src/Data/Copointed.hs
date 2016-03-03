module Data.Copointed where

import Data.Functor.Identity

class Copointed c where
  copoint :: c a -> a

instance Copointed ((,) a) where
  copoint = snd

instance Copointed Identity where
  copoint = runIdentity
