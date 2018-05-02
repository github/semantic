{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.Sum
Description : Open sums (type-indexed co-products) for extensible effects.
Copyright   : Allele Dev 2015
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

All operations are constant-time, and there is no Typeable constraint

This is a variation of OpenUnion5.hs, which relies on overlapping
instances instead of closed type families. Closed type families
have their problems: overlapping instances can resolve even
for unground types, but closed type families are subject to a
strict apartness condition.

This implementation is very similar to OpenUnion1.hs, but without
the annoying Typeable constraint. We sort of emulate it:

Our list r of open sum components is a small Universe.
Therefore, we can use the Typeable-like evidence in that
universe.

The data constructors of Sum are not exported.
-}

module Data.Sum (
  Sum,
  weakenSum,
  injectSum,
  projectSum,
  type(:<),
  type(:<:),
  Element,
  Elements,
  Apply(..),
  apply',
  apply2,
  apply2'
) where

import Data.Functor.Classes (Eq1(..), eq1, Ord1(..), compare1, Show1(..), showsPrec1)
import Data.Maybe (fromMaybe)
import Data.Sum.Templates
import GHC.Exts (Constraint)
import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits
import Unsafe.Coerce(unsafeCoerce)

pure [mkElemIndexTypeFamily 150]

infixr 5 :<

-- Strong Sum (Existential with the evidence) is an open sum
-- t is can be a GADT and hence not necessarily a Functor.
-- Int is the index of t in the list r; that is, the index of t in the
-- universe r.
data Sum (r :: [ * -> * ]) (v :: *) where
  Sum :: {-# UNPACK #-} !Int -> t v -> Sum r v

unsafeInject :: Int -> t v -> Sum r v
unsafeInject = Sum
{-# INLINE unsafeInject #-}

unsafeProject :: Int -> Sum r v -> Maybe (t v)
unsafeProject n (Sum n' x) | n == n'   = Just (unsafeCoerce x)
                           | otherwise = Nothing
{-# INLINE unsafeProject #-}

newtype P (t :: * -> *) (r :: [* -> *]) = P { unP :: Int }

infixr 5 :<:
-- | Find a list of members 'ms' in an open sum 'r'.
type family Elements ms r :: Constraint where
  Elements (t ': cs) r = (Element t r, Elements cs r)
  Elements '[] r = ()

type (ts :<: r) = Elements ts r

-- | Inject a functor into a type-aligned sum.
injectSum :: forall e r v. (e :< r) => e v -> Sum r v
injectSum = unsafeInject (unP (elemNo :: P e r))
{-# INLINE injectSum #-}

-- | Maybe project a functor out of a type-aligned sum.
projectSum :: forall e r v. (e :< r) => Sum r v -> Maybe (e v)
projectSum = unsafeProject (unP (elemNo :: P e r))
{-# INLINE projectSum #-}


weakenSum :: Sum r w -> Sum (any ': r) w
weakenSum (Sum n v) = Sum (n+1) v

type (Element t r) = KnownNat (ElemIndex t r)
type (t :< r) = Element t r

-- Find an index of an element in an `r'.
-- The element must exist, so this is essentially a compile-time computation.
elemNo :: forall t r . (t :< r) => P t r
elemNo = P (fromIntegral (natVal' (proxy# :: Proxy# (ElemIndex t r))))

-- | Helper to apply a function to a functor of the nth type in a type list.
class Apply (c :: (* -> *) -> Constraint) (fs :: [* -> *]) where
  apply :: (forall g . c g => g a -> b) -> Sum fs a -> b

apply' :: forall c fs a b . Apply c fs => (forall g . c g => (forall x. g x -> Sum fs x) -> g a -> b) -> Sum fs a -> b
apply' f u@(Sum n _) = apply @c (f (Sum n)) u
{-# INLINABLE apply' #-}

apply2 :: forall c fs a b d . Apply c fs => (forall g . c g => g a -> g b -> d) -> Sum fs a -> Sum fs b -> Maybe d
apply2 f u@(Sum n1 _) (Sum n2 r2)
  | n1 == n2  = Just (apply @c (\ r1 -> f r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing
{-# INLINABLE apply2 #-}

apply2' :: forall c fs a b d . Apply c fs => (forall g . c g => (forall x. g x -> Sum fs x) -> g a -> g b -> d) -> Sum fs a -> Sum fs b -> Maybe d
apply2' f u@(Sum n1 _) (Sum n2 r2)
  | n1 == n2  = Just (apply' @c (\ reinject r1 -> f reinject r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing
{-# INLINABLE apply2' #-}

pure (mkApplyInstance <$> [1..150])


instance Apply Foldable fs => Foldable (Sum fs) where
  foldMap f = apply @Foldable (foldMap f)
  {-# INLINABLE foldMap #-}

  foldr combine seed = apply @Foldable (foldr combine seed)
  {-# INLINABLE foldr #-}

  foldl combine seed = apply @Foldable (foldl combine seed)
  {-# INLINABLE foldl #-}

  null = apply @Foldable null
  {-# INLINABLE null #-}

  length = apply @Foldable length
  {-# INLINABLE length #-}

instance Apply Functor fs => Functor (Sum fs) where
  fmap f = apply' @Functor (\ reinject a -> reinject (fmap f a))
  {-# INLINABLE fmap #-}

  (<$) v = apply' @Functor (\ reinject a -> reinject (v <$ a))
  {-# INLINABLE (<$) #-}

instance (Apply Foldable fs, Apply Functor fs, Apply Traversable fs) => Traversable (Sum fs) where
  traverse f = apply' @Traversable (\ reinject a -> reinject <$> traverse f a)
  {-# INLINABLE traverse #-}

  sequenceA = apply' @Traversable (\ reinject a -> reinject <$> sequenceA a)
  {-# INLINABLE sequenceA #-}


instance Apply Eq1 fs => Eq1 (Sum fs) where
  liftEq eq u1 u2 = fromMaybe False (apply2 @Eq1 (liftEq eq) u1 u2)
  {-# INLINABLE liftEq #-}

instance (Apply Eq1 fs, Eq a) => Eq (Sum fs a) where
  (==) = eq1
  {-# INLINABLE (==) #-}


instance (Apply Eq1 fs, Apply Ord1 fs) => Ord1 (Sum fs) where
  liftCompare compareA u1@(Sum n1 _) u2@(Sum n2 _) = fromMaybe (compare n1 n2) (apply2 @Ord1 (liftCompare compareA) u1 u2)
  {-# INLINABLE liftCompare #-}

instance (Apply Eq1 fs, Apply Ord1 fs, Ord a) => Ord (Sum fs a) where
  compare = compare1
  {-# INLINABLE compare #-}


instance Apply Show1 fs => Show1 (Sum fs) where
  liftShowsPrec sp sl d = apply @Show1 (liftShowsPrec sp sl d)
  {-# INLINABLE liftShowsPrec #-}

instance (Apply Show1 fs, Show a) => Show (Sum fs a) where
  showsPrec = showsPrec1
  {-# INLINABLE showsPrec #-}
