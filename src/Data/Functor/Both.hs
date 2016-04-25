{-# LANGUAGE FlexibleInstances #-}
module Data.Functor.Both where

import Data.Bifunctor.Join
import Prelude hiding (zipWith, fst, snd)
import qualified Prelude

-- | A computation over both sides of a pair.
type Both a = Join (,) a

-- | Given two operands returns a functor operating on `Both`. This is a curried synonym for Both.
both :: a -> a -> Both a
both = curry Join

-- | Apply a function to `Both` sides of a computation.
runBothWith :: (a -> a -> b) -> Both a -> b
runBothWith f = uncurry f . runJoin

-- | Runs the left side of a `Both`.
fst :: Both a -> a
fst = Prelude.fst . runJoin

-- | Runs the right side of a `Both`.
snd :: Both a -> a
snd = Prelude.snd . runJoin

instance Monoid a => Monoid (Join (,) a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b
