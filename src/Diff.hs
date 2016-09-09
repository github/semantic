{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diff where

import Prologue
import Data.Functor.Foldable as Foldable
import Data.Functor.Both as Both
import Data.Mergeable
import Patch
import Syntax
import Term

-- | An annotated series of patches of terms.
type DiffF leaf annotation = FreeF (CofreeF leaf (Both annotation)) (Patch (Term leaf annotation))
type Diff a annotation = Free (CofreeF a (Both annotation)) (Patch (Term a annotation))

type SyntaxDiffF leaf annotation = DiffF (Syntax leaf) annotation
type SyntaxDiff a annotation = Diff (Syntax a) annotation

type instance Base (Free f a) = FreeF f a
instance Functor f => Foldable.Foldable (Free f a) where project = runFree
instance Functor f => Foldable.Unfoldable (Free f a) where embed = free

diffSum :: (Patch (SyntaxTerm a annotation) -> Int) -> SyntaxDiff a annotation -> Int
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: SyntaxDiff a annotation -> Int
diffCost = diffSum $ patchSum termSize

-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: (Patch (SyntaxTerm leaf annotation) -> Maybe (SyntaxTerm leaf annotation)) -> SyntaxDiff leaf annotation -> Maybe (SyntaxTerm leaf annotation)
mergeMaybe transform = iter algebra . fmap transform
  where algebra :: CofreeF (Syntax leaf) (Both annotation) (Maybe (SyntaxTerm leaf annotation)) -> Maybe (SyntaxTerm leaf annotation)
        algebra (annotations :< syntax) = cofree . (Both.fst annotations :<) <$> sequenceAlt syntax

-- | Recover the before state of a diff.
beforeTerm :: SyntaxDiff leaf annotation -> Maybe (SyntaxTerm leaf annotation)
beforeTerm = mergeMaybe before

-- | Recover the after state of a diff.
afterTerm :: SyntaxDiff leaf annotation -> Maybe (SyntaxTerm leaf annotation)
afterTerm = mergeMaybe after
