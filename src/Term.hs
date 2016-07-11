{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Term where

import Prologue
import Data.Align
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.OrderedMap hiding (size)
import Data.These
import Syntax
import GHC.Generics

-- | An annotated node (Syntax) in an abstract syntax tree.
type TermF a annotation = CofreeF (Syntax a) annotation
type Term a annotation = Cofree (Syntax a) annotation

type instance Base (Cofree f a) = CofreeF f a
instance Functor f => Foldable.Foldable (Cofree f a) where project = runCofree
instance Functor f => Foldable.Unfoldable (Cofree f a) where embed = cofree

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: Term a annotation -> Term a annotation -> Maybe (Term a (Both annotation))
zipTerms t1 t2 = annotate (zipUnwrap a b)
  where
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = fmap (cofree . (both annotation1 annotation2 :<))
    zipUnwrap (Leaf _) (Leaf b') = Just $ Leaf b'
    zipUnwrap (Indexed a') (Indexed b') = Just . Indexed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Fixed a') (Fixed b') = Just . Fixed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Keyed a') (Keyed b') | keys a' == keys b' = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a' b' <$> keys a'
    zipUnwrap _ _ = Nothing
    zipUnwrapMaps a' b' key = (,) key <$> zipTerms (a' ! key) (b' ! key)

-- | Return the node count of a term.
termSize :: Term a annotation -> Integer
termSize = cata size where
  size (_ :< syntax) = 1 + sum syntax

alignCofreeWith :: Functor f => (forall a b. f a -> f b -> Maybe (f (These a b))) -> These (Cofree f a1) (Cofree f a2) -> Free (CofreeF f (These a1 a2)) (These (Cofree f a1) (Cofree f a2))
alignCofreeWith contrast terms = fromMaybe (pure terms) $ case terms of
  These t1 t2 -> let (a1 :< s1, a2 :< s2) = (runCofree t1, runCofree t2) in
    wrap . (These a1 a2 :<) . fmap (alignCofreeWith contrast) <$> contrast s1 s2
  _ -> Nothing

alignSyntax' :: Syntax leaf a1 -> Syntax leaf a2 -> Maybe (Syntax leaf (These a1 a2))
alignSyntax' a b = case (a, b) of
  (Leaf _, Leaf s2) -> Just (Leaf s2)
  (Indexed a, Indexed b) -> Just (Indexed (align a b))
  (Fixed a, Fixed b) -> Just (Fixed (align a b))
  (Keyed a, Keyed b) -> Just (Keyed (align a b))
  _ -> Nothing


-- Generics

class Functor f => GAlign f where
  galign :: f a -> f b -> Maybe (f (These a b))

instance GAlign U1 where
  galign _ _ = Just U1

instance GAlign Par1 where
  galign (Par1 a) (Par1 b) = Just (Par1 (These a b))

instance GAlign (K1 i c) where
  galign (K1 _) (K1 b) = Just (K1 b)

instance GAlign f => GAlign (Rec1 f) where
  galign (Rec1 a) (Rec1 b) = Rec1 <$> galign a b

instance GAlign f => GAlign (M1 i c f) where
  galign (M1 a) (M1 b) = M1 <$> galign a b

instance (GAlign f, GAlign g) => GAlign (f :+: g) where
  galign = go
    where go a b = case (a, b) of
            (L1 a, L1 b) -> L1 <$> galign a b
            (R1 a, R1 b) -> R1 <$> galign a b
            _ -> Nothing

instance (GAlign f, GAlign g) => GAlign (f :*: g) where
  galign (a1 :*: b1) (a2 :*: b2) = (:*:) <$> galign a1 a2 <*> galign b1 b2

galignDefault :: (Generic1 f, GAlign (Rep1 f)) => f a -> f b -> Maybe (f (These a b))
galignDefault a b = to1 <$> galign (from1 a) (from1 b)

instance GAlign [] where
  galign = galignDefault
