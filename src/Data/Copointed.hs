module Data.Copointed where

import Prologue

-- | A value that can return its content.
class Copointed c where
  copoint :: c a -> a

instance Copointed ((,) a) where
  copoint = snd

instance Copointed Identity where
  copoint = runIdentity
