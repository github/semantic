{-# OPTIONS_GHC -fno-warn-orphans -funbox-strict-fields #-}
module Data.Functor.Both
( Both
, both
, runBothWith
, fst
, snd
, module X
) where

import Data.Bifunctor.Join as X
import Data.Functor.Classes
import Data.Semigroup
import Data.Text.Prettyprint.Doc as Pretty
import Prelude hiding (fst, snd)
import qualified Prelude

-- | A computation over both sides of a pair.
type Both = Join (,)

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


instance Eq2 p => Eq1 (Join p) where
  liftEq eq (Join a1) (Join a2) = liftEq2 eq eq a1 a2

instance Show2 p => Show1 (Join p) where
  liftShowsPrec sp sl d = showsUnaryWith (liftShowsPrec2 sp sl sp sl) "Join" d . runJoin

instance Pretty2 p => Pretty1 (Join p) where
  liftPretty p pl = liftPretty2 p pl p pl . runJoin
