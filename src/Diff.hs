{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Diff where

import Prologue
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Patch
import Syntax
import Term

-- | An annotated series of patches of terms.
type Diff a annotation = Free (CofreeF (Syntax a) (Both annotation)) (Patch (Term a annotation))

type instance Base (Free f a) = FreeF f a
instance (Functor f) => Foldable.Foldable (Free f a) where project = runFree
instance (Functor f) => Foldable.Unfoldable (Free f a) where embed = free

diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize
