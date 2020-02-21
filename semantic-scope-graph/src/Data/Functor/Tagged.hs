{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
module Data.Functor.Tagged
  ( Tagged (..)
  , identifier
  , contents
  ) where

import Control.Comonad
import Control.Lens.Getter
import Control.Lens.Lens
import Data.Function
import Data.Generics.Product
import GHC.Generics

data Tagged a = a :# !Int
  deriving (Functor, Foldable, Traversable, Generic)

infixl 7 :#

identifier :: Lens' (Tagged a) Int
identifier = position @2

contents :: Lens (Tagged a) (Tagged b) a b
contents = position @1

instance Comonad Tagged where
  extract = view contents
  duplicate (x :# tag) = x :# tag :# tag

-- This is marked as overlappable so that custom types can define their own
-- definitions of equality when wrapped in a Tagged. This may come back to
-- bite us later.
instance {-# OVERLAPPABLE #-} Eq (Tagged a) where
  (==) = (==) `on` view identifier
