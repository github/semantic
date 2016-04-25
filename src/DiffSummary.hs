{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DiffSummary where

import Diff
import Info
import Patch
import Term
import Syntax
import Data.Monoid

newtype DiffSummary = DiffSummary { diffChanges :: [DiffChange] }
  deriving (Monoid)

emptyDiffSummary :: DiffSummary
emptyDiffSummary = DiffSummary { diffChanges = [] }

data DiffChange = Insertion { category :: String, termName :: String }


patchSummary :: (Term a Info -> DiffSummary) -> Patch (Term a Info) -> DiffSummary
patchSummary termSummary patch = maybe emptyDiffSummary termSummary beforeTerm <>
  maybe emptyDiffSummary termSummary afterTerm
  where
    beforeTerm = before patch
    afterTerm = after patch

diffSummary :: Diff (Patch a) Info -> DiffSummary
diffSummary = foldMap (patchSummary termSummary)

termSummary :: Term (Patch a) Info -> DiffSummary
termSummary = cata summary where
  summary _ (Leaf (Replace _ _)) = emptyDiffSummary
  summary _ (Leaf (Insert _)) = emptyDiffSummary
  summary _ (Leaf (Delete _)) = emptyDiffSummary
  summary _ (Indexed _) = emptyDiffSummary
  summary _ (Fixed _) = emptyDiffSummary
  summary _ (Keyed _) = emptyDiffSummary
