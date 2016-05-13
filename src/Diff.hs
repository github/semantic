{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
module Diff where

import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Patch
import Syntax
import Term

-- | An annotated syntax in a diff tree.
type Annotated a annotation f = CofreeF (Syntax a) annotation f

annotation :: Annotated a annotation f -> annotation
annotation = headF

syntax :: Annotated a annotation f -> Syntax a f
syntax = tailF

annotate :: annotation -> Syntax a f -> CofreeF (Syntax a) annotation f
annotate = (:<)

-- | An annotated series of patches of terms.
type DiffF a annotation = FreeF (CofreeF (Syntax a) (Both annotation)) (Patch (Term a annotation))
type Diff a annotation = Free (CofreeF (Syntax a) (Both annotation)) (Patch (Term a annotation))

type instance Base (Free f a) = FreeF f a
instance (Functor f) => Foldable.Foldable (Free f a) where project = runFree
instance (Functor f) => Foldable.Unfoldable (Free f a) where embed = free

diffSum :: (Patch (Term a annotation) -> Integer) -> Diff a annotation -> Integer
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: Diff a annotation -> Integer
diffCost = diffSum $ patchSum termSize
