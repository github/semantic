{-# LANGUAGE TypeSynonymInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Diff where

import Control.DeepSeq
import qualified Control.Monad.Free as Free
import qualified Control.Monad.Trans.Free as FreeF
import Data.Bifunctor
import Data.Functor.Both as Both
import Data.Functor.Classes.Pretty.Generic
import Data.Mergeable
import Data.Record
import Patch
import Syntax
import Term

-- | An annotated series of patches of terms.
type DiffF f annotation = FreeF.FreeF (TermF f (Both annotation)) (Patch (Term f annotation))
type Diff f annotation = Free.Free (TermF f (Both annotation)) (Patch (Term f annotation))

type SyntaxDiff fields = Diff Syntax (Record fields)

diffSum :: (Foldable f, Functor f) => (Patch (Term f annotation) -> Int) -> Diff f annotation -> Int
diffSum patchCost diff = sum $ fmap patchCost diff

-- | The sum of the node count of the diff’s patches.
diffCost :: (Foldable f, Functor f) => Diff f annotation -> Int
diffCost = diffSum $ patchSum termSize

-- | Merge a diff using a function to provide the Term (in Maybe, to simplify recovery of the before/after state) for every Patch.
mergeMaybe :: Mergeable f => (Patch (Term f annotation) -> Maybe (Term f annotation)) -> (Both annotation -> annotation) -> Diff f annotation -> Maybe (Term f annotation)
mergeMaybe transform extractAnnotation = Free.iter algebra . fmap transform
  where algebra (annotations :<< syntax) = (extractAnnotation annotations :<) <$> sequenceAlt syntax

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
               -> Free.Free (TermF f (g annotation))  (Patch (Term f annotation))
               -> Free.Free (TermF f (g annotation')) (Patch (Term f annotation'))
mapAnnotations f = Free.hoistFree (first (fmap f)) . fmap (fmap (fmap f))

instance NFData1 f => NFData1 (Free.Free f) where
  liftRnf rnfA = go
    where go (Free.Free f) = liftRnf go f
          go (Free.Pure a) = rnfA a

instance (NFData1 f, NFData a) => NFData (Diff f a) where
  rnf = rnf1


free :: FreeF.FreeF f a (Free.Free f a) -> Free.Free f a
free (FreeF.Free f) = Free.Free f
free (FreeF.Pure a) = Free.Pure a

runFree :: Free.Free f a -> FreeF.FreeF f a (Free.Free f a)
runFree (Free.Free f) = FreeF.Free f
runFree (Free.Pure a) = FreeF.Pure a


instance Pretty1 f => Pretty1 (Free.Free f) where
  liftPretty p pl = go where go (Free.Pure a) = p a
                             go (Free.Free f) = liftPretty go (list . map (liftPretty p pl)) f

instance (Pretty1 f, Pretty a) => Pretty (Free.Free f a) where
  pretty = liftPretty pretty prettyList
