{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
-- | A functor associating an 'Int' tag value with a datum. Useful for describing unique IDs.
module Data.Functor.Tagged
  ( Tagged (..)
  , Tag
  -- * Lenses
  , identifier
  , contents
  -- * Monadic creation functions
  , taggedM
  , taggedIO
  , unsafeTagged
  -- * Reexports
  , extract
  ) where

import Control.Comonad
import Control.Effect.Fresh
import Control.Lens.Getter
import Control.Lens.Lens
import System.IO.Unsafe
import Data.Function
import Data.Generics.Product
import Data.Unique
import GHC.Generics

type Tag = Int

-- | If creating 'Tagged' values manually, it is your responsibility
-- to ensure that the provided 'Tag' is actually unique. Consider using 'taggedM'.
data Tagged a = a :# !Tag
  deriving (Functor, Foldable, Traversable, Generic)

infixl 7 :#

contents :: Lens (Tagged a) (Tagged b) a b
contents = position @1

identifier :: Lens' (Tagged a) Int
identifier = position @2

instance Show a => Show (Tagged a) where
  showsPrec n (a :# t) = showsPrec n a <> showString " # " <> showsPrec n t

-- | This is marked as overlappable so that custom types can define
-- their own definitions of equality when wrapped in a Tagged. This
-- may come back to bite us later.
instance {-# OVERLAPPABLE #-} Eq (Tagged a) where
  (==) = (==) `on` view identifier

-- | 'extract' is a handy shortcut for 'view' 'contents'
instance Comonad Tagged where
  extract = view contents
  duplicate (x :# tag) = x :# tag :# tag

-- | Tag a new value by drawing on a 'Fresh' supply.
taggedM :: Has Fresh sig m => a -> m (Tagged a)
taggedM a = (a :#) <$> fresh

-- | Tag a new value in 'IO'. The supplied values will not be numerically
-- ordered, but are guaranteed to be unique throughout the life of the program.
taggedIO :: a -> IO (Tagged a)
taggedIO a = (a :#) . hashUnique <$> newUnique

-- This is bad. Don't use it.
unsafeTagged :: a -> Tagged a
unsafeTagged = unsafePerformIO . taggedIO
{-# NOINLINE unsafeTagged #-}
