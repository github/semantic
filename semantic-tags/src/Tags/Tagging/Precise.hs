{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Tags.Tagging.Precise
( Tags
, ToTags(..)
, yield
, runTagging
, firstLine
, GFoldable1(..)
) where

import Control.Effect.Pure
import Control.Effect.Reader
import Control.Effect.Writer
import Data.Monoid (Endo(..))
import Data.Text as Text (Text, takeWhile)
import GHC.Generics
import Prelude hiding (span)
import Source.Loc (Loc(..))
import Source.Span
import Source.Source as Source
import Tags.Tag

type Tags = Endo [Tag]

class ToTags t where
  tags :: Source -> t Loc -> [Tag]


yield :: (Carrier sig m, Member (Writer Tags) sig) => Tag -> m ()
yield = tell . Endo . (:) . modSpan toOneIndexed where
  modSpan f t@Tag{ loc = l } = t { loc = l { span = f (span l) } }
  toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))

runTagging :: Source -> ReaderC Source (WriterC Tags PureC) () -> [Tag]
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
