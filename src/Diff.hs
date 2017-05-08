{-# LANGUAGE TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diff where

import Prologue
import Data.Functor.Both as Both
import Data.Mergeable
import Data.Record
import Data.These
import Patch
import Syntax
import Term

-- | An annotated series of patches of terms.
type DiffF f annotation = FreeF (TermF f (Both annotation)) (Patch (Term f annotation))
type Diff f annotation = Free (TermF f (Both annotation)) (Patch (Term f annotation))

type SyntaxDiff leaf fields = Diff (Syntax leaf) (Record fields)

diffSum :: (Foldable f, Functor f) => (Patch (Term f annotation) -> Int) -> Diff f annotation -> Int
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: (Foldable f, Functor f) => Diff f annotation -> Int
diffCost = diffSum $ patchSum termSize

-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: Mergeable f => (Patch (Term f annotation) -> Maybe (Term f annotation)) -> (Both annotation -> annotation) -> Diff f annotation -> Maybe (Term f annotation)
mergeMaybe transform extractAnnotation = iter algebra . fmap transform
  where algebra (annotations :< syntax) = cofree . (extractAnnotation annotations :<) <$> sequenceAlt syntax

-- | Recover the before state of a diff.
beforeTerm :: Mergeable f => Diff f annotation -> Maybe (Term f annotation)
beforeTerm = mergeMaybe before Both.fst

-- | Recover the after state of a diff.
afterTerm :: Mergeable f => Diff f annotation -> Maybe (Term f annotation)
afterTerm = mergeMaybe after Both.snd

-- | Map a function over the annotations in a diff, whether in diff or term nodes.
--
--   Typed using Free so as to accommodate Free structures derived from diffs that don’t fit into the Diff type synonym.
mapAnnotations :: (Functor f, Functor g)
               => (annotation -> annotation')
               -> Free (TermF f (g annotation))  (Patch (Term f annotation))
               -> Free (TermF f (g annotation')) (Patch (Term f annotation'))
mapAnnotations f = iter (wrap . first (fmap f)) . fmap (pure . fmap (fmap f))


-- | Fold a diff with a combining rule for replacement patches and an algebra on the annotated syntax functor.
foldDiffWith :: Functor f
             => (b -> b -> b) -- ^ A function to merge the results of the algebra when applied to replacement patches.
             -> (TermF f (These a a) b -> b) -- ^ An algebra on the annotated syntax functor.
             -> Diff f a -- ^ The diff to fold.
             -> b -- ^ The final resulting value.
foldDiffWith merge algebra = iter (algebra . first (runBothWith These)) . fmap (mergeTheseWith (cata algebra . fmap This) (cata algebra . fmap That) merge . unPatch)

-- | Fold a diff with an algebra on the annotated syntax functor.
--
--   This is just like 'foldDiffWith' except that it uses the overloaded '(<>)' method from 'Semigroup'.
foldDiff :: (Semigroup b, Functor f)
         => (TermF f (These a a) b -> b) -- ^ An algebra on the annotated syntax functor.
         -> Diff f a -- ^ The diff to fold.
         -> b -- ^ The final resulting value.
foldDiff = foldDiffWith (<>)


instance (NFData (f (Diff f a)), NFData (Cofree f a), NFData a, Functor f) => NFData (Diff f a) where
  rnf fa = case runFree fa of
    Free f -> rnf f `seq` ()
    Pure a -> rnf a `seq` ()
