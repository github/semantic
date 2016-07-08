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
    zipUnwrap (Comment _) (Comment b) = Just $ Comment b
    zipUnwrap (Indexed a') (Indexed b') = Indexed <$> zipWithM zipTerms a' b'
    zipUnwrap (Fixed a') (Fixed b') = Fixed <$> zipWithM zipTerms a' b'
    zipUnwrap (Keyed a') (Keyed b') | keys a' == keys b' = Just . Keyed . fromList . catMaybes $ zipUnwrapMaps a' b' <$> keys a'
    zipUnwrap (FunctionCall idA' a') (FunctionCall idB' b') = FunctionCall <$> zipTerms idA' idB' <*> zipWithM zipTerms a' b'
    zipUnwrap (Function idA' paramsA' exprsA') (Function idB' paramsB' exprsB') = case (zipTerms exprsA' exprsB') of
      Just exprs' ->  Just (Function (join $ liftA2 zipTerms idA' idB') (join $ liftA2 zipTerms paramsA' paramsB') exprs')
      _ -> Nothing
    zipUnwrap (Case eA' bodyA') (Case eB' bodyB') = Case <$> zipTerms eA' eB' <*> zipTerms bodyA' bodyB'
    zipUnwrap (Switch a' as') (Switch b' bs') = Switch <$> (zipTerms a' b') <*> zipWithM zipTerms as' bs'
    zipUnwrap (MethodCall iA mA paramsA) (MethodCall iB mB paramsB) = MethodCall <$> zipTerms iA iB <*> zipTerms mA mB <*> zipTerms paramsA paramsB
    zipUnwrap (Args a1) (Args b1) = Args <$> (zipWithM zipTerms a1 b1)
    zipUnwrap (MemberAccess a1 a2) (MemberAccess b1 b2) = MemberAccess <$> zipTerms a1 b1 <*> zipTerms a2 b2
    zipUnwrap (MathAssignment a1 a2) (MathAssignment b1 b2) = MathAssignment <$> zipTerms a1 b1 <*> zipTerms a2 b2
    zipUnwrap (Assignment a1 a2) (Assignment b1 b2) = Assignment <$> zipTerms a1 b1 <*> zipTerms a2 b2
    zipUnwrap (Ternary a1 a2) (Ternary b1 b2) = Ternary <$> zipTerms a1 b1 <*> (zipWithM zipTerms a2 b2)
    zipUnwrap (Object as') (Object bs') | as' == bs' = Object <$> zipWithM zipTerms as' bs'
    zipUnwrap (Operator as) (Operator bs) = Operator <$> zipWithM zipTerms as bs
    zipUnwrap (VarDecl a) (VarDecl b) = VarDecl <$> zipTerms a b
    zipUnwrap (VarAssignment a1 a2) (VarAssignment b1 b2) = VarAssignment <$> zipTerms a1 b1 <*> zipTerms a2 b2
    zipUnwrap (SubscriptAccess a1 a2) (SubscriptAccess b1 b2) = SubscriptAccess <$> zipTerms a1 b1 <*> zipTerms a2 b2
    zipUnwrap (Pair a1' a2') (Pair b1' b2') = Pair <$> zipTerms a1' b1' <*> zipTerms a2' b2'
    zipUnwrap (Commented cs1 a) (Commented cs2 b) = Commented <$> zipWithM zipTerms cs1 cs2 <*> (zipTerms <$> a <*> b)
    zipUnwrapMaps a' b' key = (,) key <$> zipTerms (a' ! key) (b' ! key)

-- | Return the node count of a term.
termSize :: Term a annotation -> Integer
termSize = cata size where
  size (_ :< syntax) = 1 + sum syntax
