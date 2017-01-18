{-# LANGUAGE RankNTypes, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Term where

import Prologue
import Data.Align.Generic
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.Record
import Data.These
import Syntax

-- | A Term with an abstract syntax tree and an annotation.
type Term f = Cofree f
type TermF = CofreeF

-- | A Term with a Syntax leaf and a record of fields.
type SyntaxTerm leaf fields = Term (Syntax leaf) (Record fields)
type SyntaxTermF leaf fields = TermF (Syntax leaf) (Record fields)

-- Term has a Base functor TermF which gives it Recursive and Corecursive instances.
type instance Base (Term f a) = TermF f a
instance Functor f => Recursive (Term f a) where project = runCofree
instance Functor f => Corecursive (Term f a) where embed = cofree

-- | Zip two terms by combining their annotations into a pair of annotations.
-- | If the structure of the two terms don't match, then Nothing will be returned.
zipTerms :: (Traversable f, GAlign f) => Term f annotation -> Term f annotation -> Maybe (Term f (Both annotation))
zipTerms t1 t2 = iter go (alignCofreeWith galign (const Nothing) both (These t1 t2))
  where go (a :< s) = cofree . (a :<) <$> sequenceA s

-- | Return the node count of a term.
termSize :: (Foldable f, Functor f) => Term f annotation -> Int
termSize = cata size where
  size (_ :< syntax) = 1 + sum syntax

-- | Aligns (zips, retaining non-overlapping portions of the structure) a pair of terms.
alignCofreeWith :: Functor f
  => (forall a b. f a -> f b -> Maybe (f (These a b))) -- ^ A function comparing a pair of structures, returning `Just` the combined structure if they are comparable (e.g. if they have the same constructor), and `Nothing` otherwise. The 'Data.Align.Generic.galign' function is usually what you want here.
  -> (These (Term f a) (Term f b) -> contrasted) -- ^ A function mapping a 'These' of incomparable terms into 'Pure' values in the resulting tree.
  -> (a -> b -> combined) -- ^ A function mapping the input terms’ annotations into annotations in the 'Free' values in the resulting tree.
  -> These (Term f a) (Term f b) -- ^ The input terms.
  -> Free (TermF f combined) contrasted
alignCofreeWith compare contrast combine = go
  where go terms = fromMaybe (pure (contrast terms)) $ case terms of
          These t1 t2 -> wrap . (combine (extract t1) (extract t2) :<) . fmap go <$> compare (unwrap t1) (unwrap t2)
          _ -> Nothing
