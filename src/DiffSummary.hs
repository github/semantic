{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DiffSummary where

import Diff
import Info
import Patch
import Term
import Syntax
import qualified Data.Text as T
import qualified Category
import Data.Functor.Both
import Data.Monoid
import Data.Maybe (listToMaybe)
import Data.Set (toList)
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import qualified Control.Comonad.Cofree as Cofree
import Data.Functor.Foldable as Foldable


newtype DiffSummary = DiffSummary { diffChanges :: [Patch DiffEntry] }
  deriving (Monoid)

emptyDiffSummary :: DiffSummary
emptyDiffSummary = DiffSummary { diffChanges = [] }

newtype DiffEntry = DiffEntry { termName :: String }

patchSummary :: (Term a Info -> DiffSummary) -> Patch (Term a Info) -> DiffSummary
patchSummary termSummary patch = memptyOrDiff (before patch) <> memptyOrDiff (after patch)
  where
    memptyOrDiff = maybe emptyDiffSummary termSummary

type DiffSummaryF leaf annotation = FreeF (CofreeF (Syntax leaf) (Both annotation))
diffSummary :: Diff leaf Info -> DiffSummary
-- histo :: Foldable t => (Base t (Cofree (Base t) a) -> a) -> t -> a
diffSummary = histo diffSummary' . fmap (patchSummary undefined) where
  --diffSummary' :: DiffF leaf (Cofree.Cofree (DiffF leaf Info) DiffSummary) f -> DiffSummary
  -- Skip any child that doesn't have any changes (that will always include leaves)
  -- Skip any child that doesn't have any changes (that will always include leaves)
  diffSummary' :: DiffSummaryF leaf annotation DiffSummary (Cofree.Cofree (DiffSummaryF leaf annotation DiffSummary) DiffSummary) -> DiffSummary
  diffSummary' (Free (_ :< Leaf _)) = undefined
  diffSummary' (Free (_ :< Indexed children)) = DiffSummary { diffChanges = children >>= diffChanges . extract }
  diffSummary' (Free (_ :< Fixed children)) = undefined
  diffSummary' (Free (_ :< Keyed children)) = undefined
  diffSummary' (Pure diffSummary) = diffSummary :: DiffSummary
  -- (patchSummary termSummary)

-- Syntax Text DiffSummary -> DiffSummary Text
-- If termSummary returns a DiffEntry that just contains the term name, we need to
-- Instead of foldMap we need a histomorphism

termSummary :: Term T.Text Info -> T.Text
termSummary = Foldable.cata summary where
  summary :: TermF T.Text Info f -> T.Text
  summary (info :< Leaf replace) = replace
  summary (info :< Indexed children) = toCategory info
  summary (info :< Fixed children) = toCategory info
  summary (info :< Keyed _) = toCategory info

  toCategory :: Info -> T.Text
  toCategory term = T.pack $ case maybeFirstCategory term of
    Just category -> show category
    Nothing -> "Unknown"
  maybeFirstCategory term = listToMaybe . toList $ Category.categories term
