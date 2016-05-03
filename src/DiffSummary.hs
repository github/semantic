{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DiffSummary where

import Diff
import Info
import Patch
import Term
import Syntax
import qualified Data.Text as T
import qualified Category
import Data.Monoid
import Data.Maybe (listToMaybe)
import Data.Set (toList)
import Control.Arrow
import Control.Monad
import Control.Comonad.Trans.Cofree
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

diffSummary :: Diff a Info -> DiffSummary
diffSummary = histo diffSummary' where
  diffSummary' :: DiffF a (Cofree (DiffF a Info) DiffSummary) f -> DiffSummary
  diffSummary' (coDiffSummary :< Leaf _) = diffSummary
    where (diffSummary :< _) = runCofree coDiffSummary
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
