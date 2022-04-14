{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module AST.Traversable1
( module AST.Traversable1.Class
, for1
, traverse1_
, for1_
, foldMap1
, Generics(..)
) where

import AST.Traversable1.Class
import Data.Functor (void)
import Data.Functor.Const
import Data.Monoid (Ap (..))
import GHC.Generics

for1
  :: forall c t f a b
  .  (Traversable1 c t, Applicative f)
  => t a
  -> (a -> f b)
  -> (forall t' . c t' => t' a -> f (t' b))
  -> f (t b)
for1 t f g = traverse1 @c f g t

traverse1_
  :: forall c t f a a' a''
  .  (Traversable1 c t, Applicative f)
  => (a -> f a')
  -> (forall t' . c t' => t' a -> f a'')
  -> t a
  -> f ()
traverse1_ f g = getAp . foldMap1 @c (Ap . void . f) (Ap . void . g)

for1_
  :: forall c t f a a' a''
  .  (Traversable1 c t, Applicative f)
  => t a
  -> (a -> f a')
  -> (forall t' . c t' => t' a -> f a'')
  -> f ()
for1_ t f g = getAp $ foldMap1 @c (Ap . void . f) (Ap . void . g) t

foldMap1 :: forall c t b a . (Traversable1 c t, Monoid b) => (a -> b) -> (forall t' . c t' => t' a -> b) -> t a -> b
foldMap1 f g = getConst . traverse1 @c (Const . f) (Const . g)


-- | @'Generics' t@ has a 'Traversable1' instance when @'Rep1' t@ has a 'GTraversable1' instance, making this convenient for applying 'traverse1' to 'Generic1' types lacking 'Traversable1' instances:
--
-- @
-- 'getGenerics' '<$>' 'traverse1' f g ('Generics' t) = 'to1' '<$>' 'gtraverse1' f g ('from1' t)
-- @
--
-- It further defines its 'Foldable', 'Functor', and 'Traversable' instances using 'Traversable1', making it suitable for deriving with @-XDerivingVia@.
newtype Generics t a = Generics { getGenerics :: t a }

instance (Generic1 t, GTraversable1 Foldable (Rep1 t)) => Foldable (Generics t) where
  foldMap = foldMapDefault1

instance (Generic1 t, GTraversable1 Functor (Rep1 t)) => Functor (Generics t) where
  fmap = fmapDefault1

instance (Generic1 t, GTraversable1 Foldable (Rep1 t), GTraversable1 Functor (Rep1 t), GTraversable1 Traversable (Rep1 t)) => Traversable (Generics t) where
  traverse = traverseDefault1

instance (Generic1 t, GTraversable1 c (Rep1 t)) => Traversable1 c (Generics t) where
  traverse1 f g = fmap (Generics . to1) . gtraverse1 @c f g . from1 . getGenerics
