{-# LANGUAGE FlexibleInstances #-}
module Data.Functor.Both where

import Data.Bifunctor
import Data.Bifunctor.Join
import Data.These
import Data.Maybe
import Prelude hiding (zipWith, fst, snd)
import qualified Prelude

-- | A computation over both sides of a pair.
type Both a = Join (,) a

-- | Given two operands returns a functor operating on `Both`. This is a curried synonym for Both.
both :: a -> a -> Both a
both = curry Join

-- | Construct Both (Maybe) with These values, defaulting to Nothing.
maybeBothOfThese :: These a a -> Both (Maybe a)
maybeBothOfThese = Join . fromThese Nothing Nothing . bimap Just Just

-- | Pairs either or both elements of These with the corresponding elements of Both.
-- |
-- | This is a total operation which makes it easier to cope with the lack of an Applicative or Biapplicative instance for These.
pairWithThese :: Both a -> These b c -> These (a, b) (a, c)
pairWithThese = uncurry bimap . bimap (,) (,) . runBoth

-- | Extract `Both` sides of a computation.
runBoth :: Both a -> (a, a)
runBoth = runJoin

-- | Apply a function to `Both` sides of a computation.
runBothWith :: (a -> a -> b) -> Both a -> b
runBothWith f = uncurry f . runBoth

-- | Runs the left side of a `Both`.
fst :: Both a -> a
fst = Prelude.fst . runBoth

-- | Runs the right side of a `Both`.
snd :: Both a -> a
snd = Prelude.snd . runBoth

instance Monoid a => Monoid (Join (,) a) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b
