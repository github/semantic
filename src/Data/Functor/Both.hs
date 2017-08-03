{-# OPTIONS_GHC -fno-warn-orphans -funbox-strict-fields #-}
module Data.Functor.Both (Both, both, runBothWith, fst, snd, module X) where

import Control.DeepSeq
import Data.Bifunctor.Join as X
import Data.Semigroup
import Prelude hiding (fst, snd)
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

instance (Semigroup a, Monoid a) => Monoid (Join (,) a) where
  mempty = pure mempty
  mappend = (<>)


instance (Semigroup a) => Semigroup (Join (,) a) where
  a <> b = Join $ runJoin a <> runJoin b

instance NFData a => NFData (Join (,) a)
