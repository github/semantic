{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diff where

import Prologue
import Data.Functor.Foldable as Foldable
import Data.Functor.Both as Both
import Patch
import Syntax
import Term

-- | An annotated series of patches of terms.
type DiffF leaf annotation = FreeF (CofreeF (Syntax leaf) (Both annotation)) (Patch (Term leaf annotation))
type Diff a annotation = Free (CofreeF (Syntax a) (Both annotation)) (Patch (Term a annotation))

type instance Base (Free f a) = FreeF f a
instance Functor f => Foldable.Foldable (Free f a) where project = runFree
instance Functor f => Foldable.Unfoldable (Free f a) where embed = free

diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize

-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: (Patch (Term leaf annotation) -> Maybe (Term leaf annotation)) -> Diff leaf annotation -> Maybe (Term leaf annotation)
mergeMaybe transform = iter algebra . fmap transform
  where algebra :: CofreeF (Syntax leaf) (Both annotation) (Maybe (Term leaf annotation)) -> Maybe (Term leaf annotation)
        algebra (annotations :< syntax) = Just . cofree $ Both.fst annotations :< case syntax of
          Leaf s -> Leaf s
          Indexed i -> Indexed (catMaybes i)
          Fixed i -> Fixed (catMaybes i)

-- | Recover the before state of a diff.
beforeTerm :: Diff leaf annotation -> Maybe (Term leaf annotation)
beforeTerm = mergeMaybe before

-- | Recover the after state of a diff.
afterTerm :: Diff leaf annotation -> Maybe (Term leaf annotation)
afterTerm = mergeMaybe after
