{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, RecordWildCards #-}
module DiffSummary where

import Diff
import Info
import Patch
import Term
import Syntax
import qualified Category
import Data.Functor.Both
import Data.Monoid
import Data.Maybe (listToMaybe)
import Data.Set (toList)
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import qualified Control.Comonad.Cofree as Cofree
import Data.Functor.Foldable as Foldable

newtype DiffContext = DiffContext { modules :: [String] }
  deriving (Monoid)

data DiffSummary = DiffSummary { beforeSummary :: DiffSummary, afterSummary :: DiffSummary, context :: Maybe DiffContext } | TermSummary { termName :: String } | EmptySummary

  -- T { 1 }
  -- T { 2 }
  -- T {1, 2}
  -- T {'array', 'dictionary'}
  -- T {Nothing, 'dictionary'}

  -- Given two (Both (Maybe String))
  -- TermSummary { name1 :: Maybe String, name2 :: Maybe String }
  -- TermSummary { name1 = Just "1", name2 :: Just "2" }
  -- DiffSummary { beforeSummary = (TermSummary { name1 = Just "1"}), afterSummary = (TermSummary { name = Just "2"}), diffContext = Nothing }

instance (Monoid DiffSummary)  where
  mempty = EmptySummary
  mappend EmptySummary EmptySummary = EmptySummary
  mappend EmptySummary summary@TermSummary{..} = summary
  mappend s@TermSummary{} EmptySummary = s
  mappend EmptySummary summary@DiffSummary{..} = summary
  mappend s1@TermSummary{} s2@TermSummary{} = DiffSummary { beforeSummary = s1, afterSummary = s2, context = Nothing }
  mappend s@TermSummary{} DiffSummary{..} = DiffSummary { beforeSummary = mappend s beforeSummary, afterSummary = afterSummary, context = context }
  mappend DiffSummary{..} s@TermSummary{} = DiffSummary { beforeSummary = beforeSummary, afterSummary = mappend afterSummary s, context = context }
  mappend summary@DiffSummary{} EmptySummary = summary
  mappend s1@DiffSummary{} s2@DiffSummary{} = DiffSummary { beforeSummary = mappend (beforeSummary s1) (beforeSummary s2), afterSummary = mappend (afterSummary s1) (afterSummary s2), context = mappend (context s1) (context s2) }

emptyDiffSummary :: DiffSummary
emptyDiffSummary = EmptySummary

patchToSummary :: (Term a Info -> DiffSummary) -> Patch (Term a Info) -> DiffSummary
patchToSummary termSummary patch = memptyOrDiff (before patch) <> memptyOrDiff (after patch)
  where
    memptyOrDiff = maybe emptyDiffSummary termSummary

type DiffSummaryF leaf annotation = FreeF (CofreeF (Syntax leaf) (Both annotation))

diffSummary :: Diff leaf Info -> DiffSummary
-- histo :: Foldable t => (Base t (Cofree (Base t) a) -> a) -> t -> a
diffSummary = histo diffSummary' . fmap (patchToSummary termToSummary) where
  --diffSummary' :: DiffF leaf (Cofree.Cofree (DiffF leaf Info) DiffSummary) f -> DiffSummary
  -- Skip any child that doesn't have any changes (that will always include leaves)
  diffSummary' :: DiffSummaryF leaf annotation DiffSummary (Cofree.Cofree (DiffSummaryF leaf annotation DiffSummary) DiffSummary) -> DiffSummary
  diffSummary' (Free (_ :< Leaf _)) = undefined
  diffSummary' (Free (_ :< Indexed children)) = DiffSummary {}
  diffSummary' (Free (_ :< Fixed children)) = undefined
  diffSummary' (Free (_ :< Keyed children)) = undefined
  diffSummary' (Pure diffSummary) = diffSummary :: DiffSummary
  -- (patchSummary termSummary)

-- Syntax Text DiffSummary -> DiffSummary Text
-- If termSummary returns a DiffEntry that just contains the term name, we need to
-- Instead of foldMap we need a histomorphism

termToSummary :: Term leaf Info -> DiffSummary
termToSummary = Foldable.cata summary where
  summary :: TermF leaf Info f -> DiffSummary
  summary (info :< Leaf replace) = toCategory info
  summary (info :< Indexed children) = toCategory info
  summary (info :< Fixed children) = toCategory info
  summary (info :< Keyed _) = toCategory info

  toCategory :: Info -> DiffSummary
  toCategory term = case maybeFirstCategory term of
    Just category -> TermSummary { termName = show category }
    Nothing -> EmptySummary
  maybeFirstCategory term = listToMaybe . toList $ Category.categories term
