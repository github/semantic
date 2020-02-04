{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module defines the 'Traversable1' class and its generic derivation using 'GTraversable1'. Note that any changes to this file will require recompilation of all of the AST modules, which is quite expensive; thus, most additions should be made in "AST.Traversable1" instead, and that that module should not be imported by the AST modules.
module AST.Traversable1.Class
( Traversable1(..)
, foldMapDefault1
, fmapDefault1
, traverseDefault1
, GTraversable1(..)
) where

import Data.Functor.Const
import Data.Functor.Identity
import GHC.Generics

-- | Simultaneous traversal of subterms of kind @*@ and @* -> *@ in an 'Applicative' context.
--
-- 'Traversable1' can express any combination of first- and second-order mapping, folding, and traversal.
--
-- Note that the @1@ suffix is used in the manner of 'Data.Functor.Classes.Show1' or 'Generic1', rather than 'foldr1'; it’s a higher-order traversal which is simultaneously able to traverse (and alter) annotations.
class Traversable1 c t where
  -- | Map annotations of kind @*@ and heterogeneously-typed subterms of kind @* -> *@ under some constraint @c@ into an 'Applicative' context. The constraint is necessary to operate on otherwise universally-quantified subterms, since otherwise there would be insufficient information to inspect them at all.
  --
  -- No proxy is provided for the constraint @c@; instead, @-XTypeApplications@ should be used. E.g. here we ignore the annotations and print all the @* -> *@ subterms using 'Show1':
  --
  -- @
  -- 'traverse1' \@'Data.Functor.Classes.Show1' 'pure' (\ t -> t '<$' 'putStrLn' ('Data.Functor.Classes.showsPrec1' 0 t ""))
  -- @
  --
  -- Note that this traversal is non-recursive: any recursion through subterms must be performed by the second function argument.
  traverse1
    :: Applicative f
    => (a -> f b)
    -> (forall t' . c t' => t' a -> f (t' b))
    -> t a
    -> f (t b)
  default traverse1
    :: (Applicative f, Generic1 t, GTraversable1 c (Rep1 t))
    => (a -> f b)
    -> (forall t' . c t' => t' a -> f (t' b))
    -> t a
    -> f (t b)
  traverse1 f g = fmap to1 . gtraverse1 @c f g . from1


-- | This function may be used as a value for 'foldMap' in a 'Foldable' instance.
foldMapDefault1 :: (Traversable1 Foldable t, Monoid b) => (a -> b) -> t a -> b
foldMapDefault1 f = getConst . traverse1 @Foldable (Const . f) (Const . foldMap f)

-- | This function may be used as a value for 'fmap' in a 'Functor' instance.
fmapDefault1 :: Traversable1 Functor t => (a -> b) -> t a -> t b
fmapDefault1 f = runIdentity . traverse1 @Functor (Identity . f) (Identity . fmap f)

-- | This function may be used as a value for 'traverse' in a 'Traversable' instance.
traverseDefault1 :: (Traversable1 Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverseDefault1 f = traverse1 @Traversable f (traverse f)


class GTraversable1 c t where
  -- | Generically map annotations and subterms of kind @* -> *@ into an 'Applicative' context.
  gtraverse1
    :: Applicative f
    => (a -> f b)
    -> (forall t' . c t' => t' a -> f (t' b))
    -> t a
    -> f (t b)

instance GTraversable1 c f => GTraversable1 c (M1 i c' f) where
  gtraverse1 f g = fmap M1 . gtraverse1 @c f g . unM1

instance (GTraversable1 c f, GTraversable1 c g) => GTraversable1 c (f :*: g) where
  gtraverse1 f g (l :*: r) = (:*:) <$> gtraverse1 @c f g l <*> gtraverse1 @c f g r

instance (GTraversable1 c f, GTraversable1 c g) => GTraversable1 c (f :+: g) where
  gtraverse1 f g (L1 l) = L1 <$> gtraverse1 @c f g l
  gtraverse1 f g (R1 r) = R1 <$> gtraverse1 @c f g r

instance GTraversable1 c (K1 R t) where
  gtraverse1 _ _ (K1 k) = pure (K1 k)

instance GTraversable1 c Par1 where
  gtraverse1 f _ (Par1 a) = Par1 <$> f a

instance c t => GTraversable1 c (Rec1 t) where
  gtraverse1 _ g (Rec1 t) = Rec1 <$> g t

instance (Traversable f, GTraversable1 c g) => GTraversable1 c (f :.: g) where
  gtraverse1 f g = fmap Comp1 . traverse (gtraverse1 @c f g) . unComp1

instance GTraversable1 c U1 where
  gtraverse1 _ _ _ = pure U1
