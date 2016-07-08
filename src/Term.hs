{-# LANGUAGE TypeFamilies, TypeSynonymInstances, Unsafe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Term where

import Prologue
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.OrderedMap hiding (size)
import Syntax
import Unsafe

-- | An annotated node (Syntax) in an abstract syntax tree.
type TermF a annotation = CofreeF (Syntax a) annotation
type Term a annotation = Cofree (Syntax a) annotation

type instance Base (Cofree f a) = CofreeF f a
instance Functor f => Foldable.Foldable (Cofree f a) where project = runCofree
instance Functor f => Foldable.Unfoldable (Cofree f a) where embed = cofree

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: (Eq a, Eq annotation) => Term a annotation -> Term a annotation -> Maybe (Term a (Both annotation))
zipTerms t1 t2 = annotate (zipUnwrap a b)
  where
    (annotation1 :< a, annotation2 :< b) = (runCofree t1, runCofree t2)
    annotate = fmap (cofree . (both annotation1 annotation2 :<))
    zipUnwrap (Leaf _) (Leaf b') = Just $ Leaf b'
    zipUnwrap (Indexed a') (Indexed b') = Just . Indexed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (FunctionCall idA' a') (FunctionCall idB' b') = case (zipTerms idA' idB') of
      (Just id') ->  Just $ FunctionCall id' (catMaybes $ zipWith zipTerms a' b')
      (_) -> Nothing
    zipUnwrap (Function idA' paramsA' exprsA') (Function idB' paramsB' exprsB') = case (zipTerms exprsA' exprsB') of
      Just exprs' ->  Just (Function (join $ liftA2 zipTerms idA' idB') (join $ liftA2 zipTerms paramsA' paramsB') exprs')
      _ -> Nothing
    zipUnwrap (Case eA' bodyA') (Case eB' bodyB') = case (zipTerms eA' eB', zipTerms bodyA' bodyB') of
      (Just id', Just body') -> Just $ Case id' body'
      _ -> Nothing
    zipUnwrap (Switch a' as') (Switch b' bs') = case (zipTerms a' b') of
      (Just expr') -> Just $ Switch expr' (catMaybes $ zipWith zipTerms as' bs')
      _ -> Nothing
    zipUnwrap (Object as') (Object bs') | as' == bs' = Just . Object . catMaybes $ zipWith zipTerms as' bs'
    zipUnwrap (Fixed a') (Fixed b') = Just . Fixed . catMaybes $ zipWith zipTerms a' b'
    zipUnwrap (Keyed a') (Keyed b') | keys a' == keys b' = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a' b' <$> keys a'
    zipUnwrap _ _ = Nothing
    zipUnwrapMaps a' b' key = (,) key <$> zipTerms (a' ! key) (b' ! key)

-- | Return the node count of a term.
termSize :: Term a annotation -> Integer
termSize = cata size where
  size (_ :< syntax) = 1 + sum syntax
