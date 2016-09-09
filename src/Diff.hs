{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diff where

import Prologue
import Data.Functor.Foldable as Foldable
import Data.Functor.Both as Both
import Data.Mergeable
import Patch
import Term

-- | An annotated series of patches of terms.
type DiffF f annotation = FreeF (TermF f (Both annotation)) (Patch (Term f annotation))
type Diff f annotation = Free (TermF f (Both annotation)) (Patch (Term f annotation))

type instance Base (Free f a) = FreeF f a
instance Functor f => Foldable.Foldable (Free f a) where project = runFree
instance Functor f => Foldable.Unfoldable (Free f a) where embed = free

diffSum :: (Prologue.Foldable f, Functor f) => (Patch (Term f annotation) -> Int) -> Diff f annotation -> Int
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: (Prologue.Foldable f, Functor f) => Diff f annotation -> Int
diffCost = diffSum $ patchSum termSize

-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: (Functor f, Mergeable f) => (Patch (Term f annotation) -> Maybe (Term f annotation)) -> Diff f annotation -> Maybe (Term f annotation)
mergeMaybe transform = iter algebra . fmap transform
  where algebra :: Mergeable f => TermF f (Both annotation) (Maybe (Term f annotation)) -> Maybe (Term f annotation)
        algebra (annotations :< syntax) = cofree . (Both.fst annotations :<) <$> sequenceAlt syntax

-- | Recover the before state of a diff.
beforeTerm :: (Functor f, Mergeable f) => Diff f annotation -> Maybe (Term f annotation)
beforeTerm = mergeMaybe before

-- | Recover the after state of a diff.
afterTerm :: (Functor f, Mergeable f) => Diff f annotation -> Maybe (Term f annotation)
afterTerm = mergeMaybe after
