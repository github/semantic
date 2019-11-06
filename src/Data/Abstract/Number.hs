{-# LANGUAGE GADTs, StandaloneDeriving, RankNTypes, TypeApplications #-}

module Data.Abstract.Number
    ( Number (..)
    , SomeNumber (..)
    , liftReal
    , liftIntegralFrac
    , liftedExponent
    , liftedFloorDiv
    ) where

import Data.Scientific
import qualified Prelude
import Prelude hiding (Integer)
import Prologue

-- | A generalized number type that unifies all interpretable numeric types.
--   This is a GADT, so you can specialize the 'a' parameter and be confident
--   that, say, a @Number Scientific@ contains a 'Scientific' and not an integer
--   in disguise. This unified type is used to provide mathematical operations
--   that can change their representation based on an operation's operands—e.g.
--   raising a rational number to a ratio may not produce another rational number.
--   This also neatly encapsulates the "coalescing" behavior of adding numbers
--   of different type in dynamic languages: operating on a whole and a rational
--   produces a rational, operating on a rational and a decimal produces a decimal,
--   and so on and so forth. When we add complex numbers, they will in turn subsume
--   the other numeric types.
data Number a where
  Integer :: !Prelude.Integer  -> Number Prelude.Integer
  Ratio   :: !Prelude.Rational -> Number Prelude.Rational
  Decimal :: !Scientific       -> Number Scientific

deriving instance Eq a => Eq (Number a)

instance Show (Number a) where
  show (Integer i) = show i
  show (Ratio r) = show r
  show (Decimal d) = show d

-- | Every 'Number' can be coerced to a 'Scientific'. Used in the 'Ord' instance.
toScientific :: Number a -> Scientific
toScientific (Integer i) = fromInteger i
toScientific (Ratio r) = fromRational r
toScientific (Decimal s) = s

instance Eq a => Ord (Number a) where compare = compare `on` toScientific

-- | A box that hides the @a@ parameter to a given 'Number'. Pattern-match
--   on it to extract the information contained; because there are only three
--   possible constructors, pattern-matching all three cases is possible.
data SomeNumber = forall a . SomeNumber (Number a)

-- | Smart constructors for 'SomeNumber'.
whole :: Prelude.Integer -> SomeNumber
whole = SomeNumber . Integer

ratio :: Prelude.Rational -> SomeNumber
ratio = SomeNumber . Ratio

decim :: Scientific -> SomeNumber
decim = SomeNumber . Decimal

-- | In order to provide truly generic math operations, where functions like
--   exponentiation handle the fact that they are not closed over the rational
--   numbers, we must promote standard Haskell math functions from operations
--   on 'Real', 'Integral', and 'Fractional' numbers into functions that operate
--   on two 'Number' values and return a temporarily-indeterminate 'SomeNumber'
--   value. At the callsite, we can then unwrap the 'SomeNumber' and handle the
--   specific cases.
--
--   Promote a function on 'Real' values into one operating on 'Number's.
--   You pass things like @+@ and @-@ here.
liftReal :: (forall n . Real n => n -> n -> n)
         -> (Number a -> Number b -> SomeNumber)
liftReal f = liftIntegralFrac f f

-- | Promote two functions, one on 'Integral' and one on 'Fractional' and 'Real' values,
--   to operate on 'Numbers'. Examples of this: 'mod' and 'mod'', 'div' and '/'.
liftIntegralFrac :: (forall n . Integral n             => n -> n -> n)
                 -> (forall f . (Fractional f, Real f) => f -> f -> f)
                 -> (Number a -> Number b -> SomeNumber)
liftIntegralFrac f _ (Integer i) (Integer j) = whole (f i j)
liftIntegralFrac _ g (Integer i) (Ratio j)   = ratio (g (toRational i) j)
liftIntegralFrac _ g (Integer i) (Decimal j) = decim (g (fromIntegral i) j)
liftIntegralFrac _ g (Ratio i) (Ratio j)     = ratio (g i j)
liftIntegralFrac _ g (Ratio i) (Integer j)   = ratio (g i (fromIntegral j))
liftIntegralFrac _ g (Ratio i) (Decimal j)   = decim (g (fromRational i) j)
liftIntegralFrac _ g (Decimal i) (Integer j) = decim (g i (fromIntegral j))
liftIntegralFrac _ g (Decimal i) (Ratio j)   = decim (g i (fromRational j))
liftIntegralFrac _ g (Decimal i) (Decimal j) = decim (g i j)

-- | Exponential behavior is too hard to generalize, so here's a manually implemented version.
--   TODO: Given a 'Ratio' raised to some 'Integer', we could check to see if it's an integer
--   and round it before the exponentiation, giving back a 'Integer'.
liftedExponent :: Number a -> Number b -> SomeNumber
liftedExponent (Integer i) (Integer j) = whole (i ^ j)
liftedExponent (Ratio i) (Integer j)   = ratio (i ^^ j)
liftedExponent i j                     = decim (fromFloatDigits (munge i ** munge j))
  where munge = (toRealFloat . toScientific) :: Number a -> Double

liftedFloorDiv :: Number a -> Number b -> SomeNumber
liftedFloorDiv (Integer i) (Integer j) = whole (i `div` j)
liftedFloorDiv i j                     = decim (fromIntegral @Prelude.Integer (floor (fromFloatDigits (munge i / munge j))))
  where munge = (toRealFloat . toScientific) :: Number a -> Double
