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


newtype DiffSummary = DiffSummary { diffChanges :: [Patch DiffEntry] }
  deriving (Monoid)

emptyDiffSummary :: DiffSummary
emptyDiffSummary = DiffSummary { diffChanges = [] }

newtype DiffEntry = DiffEntry { termName :: String }

patchSummary :: (Term a Info -> DiffSummary) -> Patch (Term a Info) -> DiffSummary
patchSummary termSummary patch = memptyOrDiff (before patch) <> memptyOrDiff (after patch)
  where
    memptyOrDiff = maybe emptyDiffSummary termSummary

diffSummary :: Diff (Patch a) Info -> DiffSummary
diffSummary = foldMap (patchSummary termSummary)

-- Syntax Text DiffSummary -> DiffSummary Text

termSummary :: Term T.Text Info -> T.Text
termSummary = cata summary where
  summary :: Info -> Syntax T.Text f -> T.Text
  summary info (Leaf replace) = replace
  summary info (Indexed children) = toCategory info
  summary info (Fixed children) = toCategory info
  summary info (Keyed _) = toCategory info

  toCategory :: Info -> T.Text
  toCategory term = T.pack $ case listToMaybe . toList $ Category.categories term of
    Just category -> show category
    Nothing -> "Unknown"
