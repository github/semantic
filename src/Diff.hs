{-# LANGUAGE TypeFamilies, TypeSynonymInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diff where

import Prologue
import Data.Functor.Foldable as F
import Data.Functor.Both as Both
import Data.Mergeable
import Data.Record
import Patch
import Syntax
import Term

-- | An annotated series of patches of terms.
type DiffF f annotation = FreeF (TermF f (Both annotation)) (Patch (Term f annotation))
type Diff f annotation = Free (TermF f (Both annotation)) (Patch (Term f annotation))

type SyntaxDiff leaf fields = Diff (Syntax leaf) (Record fields)

type instance Base (Free f a) = FreeF f a
instance Functor f => Recursive (Free f a) where project = runFree
instance Functor f => Corecursive (Free f a) where embed = free

diffSum :: (Foldable f, Functor f) => (Patch (Term f annotation) -> Int) -> Diff f annotation -> Int
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: (Foldable f, Functor f) => Diff f annotation -> Int
diffCost = diffSum $ patchSum termSize

-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: forall f annotation. Mergeable f => (Patch (Term f annotation) -> Maybe (Term f annotation)) -> (Both annotation -> annotation) -> Diff f annotation -> Maybe (Term f annotation)
mergeMaybe transform extractAnnotation = iter algebra . fmap transform
  where algebra :: TermF f (Both annotation) (Maybe (Term f annotation)) -> Maybe (Term f annotation)
        algebra (annotations :< syntax) = cofree . (extractAnnotation annotations :<) <$> sequenceAlt syntax

-- | Recover the before state of a diff.
beforeTerm :: Mergeable f => Diff f annotation -> Maybe (Term f annotation)
beforeTerm = mergeMaybe before Both.fst

-- | Recover the after state of a diff.
afterTerm :: Mergeable f => Diff f annotation -> Maybe (Term f annotation)
afterTerm = mergeMaybe after Both.snd


-- | Map a function over the annotations in a diff, whether in diff or term nodes.
mapAnnotations :: Functor f => (annotation -> annotation') -> Diff f annotation -> Diff f annotation'
mapAnnotations f = iter (\ (h :< functor) -> wrap (fmap f h :< functor)) . fmap (pure . fmap (fmap f))
