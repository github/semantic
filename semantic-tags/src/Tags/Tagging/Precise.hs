{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Tags.Tagging.Precise
( Tags
, ToTags(..)
, yield
, runTagging
, firstLine
, GFoldable1(..)
, Traversable1(..)
, foldMap1
, GTraversable1(..)
, Generics(..)
) where

import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid (Endo (..))
import Data.Text as Text (Text, takeWhile)
import GHC.Generics
import Prelude hiding (span)
import Source.Loc (Loc (..))
import Source.Source as Source
import Source.Span
import Tags.Tag

type Tags = Endo [Tag]

class ToTags t where
  tags :: Source -> t Loc -> [Tag]


yield :: Has (Writer Tags) sig m => Tag -> m ()
yield = tell . Endo . (:) . modSpan toOneIndexed where
  modSpan f t@Tag{ loc = l } = t { loc = l { span = f (span l) } }
  toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))

runTagging :: Source -> ReaderC Source (WriterC Tags Identity) () -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader source

firstLine :: Source -> Text
firstLine = Text.takeWhile (/= '\n') . toText . Source.take 180


-- FIXME: move GFoldable1 into semantic-ast.
class GFoldable1 c t where
  -- | Generically map functions over fields of kind @* -> *@, monoidally combining the results.
  gfoldMap1
    :: Monoid b
    => (forall f . c f => f a -> b)
    -> t a
    -> b

instance GFoldable1 c f => GFoldable1 c (M1 i c' f) where
  gfoldMap1 alg = gfoldMap1 @c alg . unM1

instance (GFoldable1 c f, GFoldable1 c g) => GFoldable1 c (f :*: g) where
  gfoldMap1 alg (f :*: g) = gfoldMap1 @c alg f <> gfoldMap1 @c alg g

instance (GFoldable1 c f, GFoldable1 c g) => GFoldable1 c (f :+: g) where
  gfoldMap1 alg (L1 l) = gfoldMap1 @c alg l
  gfoldMap1 alg (R1 r) = gfoldMap1 @c alg r

instance GFoldable1 c (K1 R t) where
  gfoldMap1 _ _ = mempty

instance GFoldable1 c Par1 where
  gfoldMap1 _ _ = mempty

instance c t => GFoldable1 c (Rec1 t) where
  gfoldMap1 alg (Rec1 t) = alg t

instance (Foldable f, GFoldable1 c g) => GFoldable1 c (f :.: g) where
  gfoldMap1 alg = foldMap (gfoldMap1 @c alg) . unComp1

instance GFoldable1 c U1 where
  gfoldMap1 _ _ = mempty


class Traversable1 c t where
  -- | Map annotations and subterms of kind @* -> *@ into an 'Applicative' context.
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

foldMap1 :: forall c t b a . (Traversable1 c t, Monoid b) => (a -> b) -> (forall t' . c t' => t' a -> b) -> t a -> b
foldMap1 f g = getConst . traverse1 @c (Const . f) (Const . g)


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


newtype Generics t a = Generics { getGenerics :: t a }
  deriving (Foldable, Functor, Traversable)

instance (Generic1 t, GTraversable1 c (Rep1 t)) => Traversable1 c (Generics t) where
  traverse1 f g = fmap (Generics . to1) . gtraverse1 @c f g . from1 . getGenerics
