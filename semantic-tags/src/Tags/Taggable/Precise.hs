{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Tags.Taggable.Precise
( runTagging
, Tags
, ToTags(..)
, yield
, GFoldable1(..)
) where

import Control.Effect.Reader
import Control.Effect.Writer
import Data.Monoid (Endo(..))
import GHC.Generics
import Source.Loc
import Source.Source
import Tags.Tag

runTagging :: ToTags t => Source -> t Loc -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader source
  . tags

type Tags = Endo [Tag]

class ToTags t where
  tags
    :: ( Carrier sig m
       , Member (Reader Source) sig
       , Member (Writer Tags) sig
       )
    => t Loc
    -> m ()


yield :: (Carrier sig m, Member (Writer Tags) sig) => Tag -> m ()
yield = tell . Endo . (:)


class GFoldable1 c t where
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
