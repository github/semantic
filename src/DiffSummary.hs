{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, RecordWildCards, TypeFamilies #-}
module DiffSummary where

import Prelude hiding (fst, snd)
import Diff
import Info
import Patch
import Term
import Syntax
import qualified Range as R
import Category as Category
import Data.Functor.Both
import Data.Monoid
import Data.Maybe (listToMaybe)
import Data.Set (toList)
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Cofree as Cofree
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable as F
import Control.Monad.State hiding (sequence)
import qualified Data.Map as M
import Data.Functor.Identity

data DiffSummary a = TermSummary {
  description :: String,
  annotation :: a,
  parentAnnotations :: [a]
}
  deriving (Eq, Show, Functor, Ord)

data instance Prim (DiffSummary a) b = PBranchSummary a b | PTermSummary String a b | PParentSummary a
  deriving (Show, Functor)

type instance Base (DiffSummary a) = Prim (DiffSummary a)
-- instance Unfoldable (DiffSummary a) where
--   embed (PTermSummary s a b) = TermSummary s a b
--   embed (PParentSummary a) = ParentSummary a

-- data DiffSummary' = [(String, [String])]

-- data DiffSummary'' = DiffSummaryForPatch'' String | DiffSummaryForBranch'' String [DiffSummary'']

-- x = DiffSummaryForBranch'' "module Foo" [ DiffSummaryForPatch'' "inserted thing" ] 
-- x = DiffSummaryForBranch'' "module Foo" [ DiffSummaryForBranch'' "module Bar" [ DiffSummaryForPatch'' "deleted thing" ], DiffSummaryForPatch'' "inserted thing" ]

-- data Context = DiffContext DiffSummary
--   deriving (Show)

-- data DiffResult = DiffResult {
--   assumptions :: M.Map String [DiffSummary]
-- } deriving (Show)

-- instance Monoid DiffResult where
--   mempty = DiffResult mempty
--   mappend a b = DiffResult (assumptions a `mappend` assumptions b)

-- newtype DiffState t m = DiffState {
--   memo :: M.Map t m
-- }

-- type Summarize t = State (DiffState t (DiffSummary, DiffResult)) (DiffSummary, DiffResult)

-- memoSummarize :: Ord c => (c -> Summarize c) -> c -> Summarize c
-- memoSummarize f c = gets memo >>= maybe memoize return . M.lookup c where
--   memoize = do
--     r <- f c
--     modify $ \s -> s { memo = M.insert c r $ memo s }
--     return r

-- generateSummaries :: DiffF leaf Info f -> Summarize (Diff leaf Info)
-- generateSummaries (Pure patch) = return (DEmptySummary, mempty)
-- generateSummaries (Free (_ :< Leaf _)) = return (DEmptySummary, mempty)
-- generateSummaries (Free (info :< Indexed f)) = do 
--   childStates <- _
--   -- * --
--   --     * --
--   --         *

  -- let childContexts = maybe [] (map $ DiffContext _) (M.lookup key . assumptions $ snd childState)
  -- let as = M.delete key . assumptions $ snd childState
  -- let diffResult = DiffResult {
  --       assumptions = _ }
  -- return (toCategory (snd info), diffResult)
  -- T { 1 }
  -- T { 2 }
  -- T {1, 2}
  -- T {'array', 'dictionary'}
  -- T {Nothing, 'dictionary'}

  -- Given two (Both (Maybe String))
  -- TermSummary { name1 :: Maybe String, name2 :: Maybe String }
  -- TermSummary { name1 = Just "1", name2 :: Just "2" }
  -- DiffSummary { beforeSummary = (TermSummary { name1 = Just "1"}), afterSummary = (TermSummary { name = Just "2"}), diffContext = Nothing }

info :: Info
info = Info (R.rangeAt 0) mempty 1

eLeaf :: Diff String Info
eLeaf = retract . free . Pure . Insert . cofree $ info :< Leaf "a"

freeLeaf :: Diff String Info
freeLeaf = free . Free $ (pure info :< Indexed [free . Free $ (pure info :< Leaf "a"), free $ Pure (Insert $ cofree (info :< Leaf "b"))])

eIndexed :: Diff String Info
eIndexed = free . Pure . Insert . cofree $ info :< Indexed [cofree $ info :< Leaf "a"]

patchToSummary :: (Term a Info -> DiffSummary a) -> Patch (Term a Info) -> DiffSummary a
patchToSummary termSummary patch = undefined -- memptyOrDiff (before patch) <> memptyOrDiff (after patch)

diffSummary :: Diff leaf Info -> [DiffSummary ()]
-- histo :: Foldable t => (Base t (Cofree (Base t) a) -> a) -> t -> a
diffSummary = cata diffSummary' where
  diffSummary' :: DiffF leaf Info [DiffSummary ()] -> [DiffSummary ()]
  -- Skip any child that doesn't have any changes (that will always include leaves)
  -- Prune leaves
  diffSummary' (Free (info :< Leaf _)) = []
  -- Return a contextless indexed summary with it's indexed context distributed to its children
  diffSummary' (Free (_ :< Indexed children)) = prependSummary () <$> join children
  -- Return a contextless fixed diff summary with it's fixed context distributed to its children
  diffSummary' (Free (_ :< Fixed children)) = prependSummary () <$> join children
  diffSummary' (Free (_ :< Keyed children)) = prependSummary () <$> join (F.toList children)
  -- Return a contextless diff summary
  diffSummary' (Pure (Insert term)) = [TermSummary "insert" () []]
  diffSummary' (Pure (Delete term)) = [TermSummary "delete" () []]
  diffSummary' (Pure (Replace t1 t2)) = [TermSummary "replace" () []]

prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }
  -- (patchSummary termSummary)

-- diffSummary'' :: Diff leaf Info -> DiffSummary ()
-- diffSummary'' = hylo combineSummaries tearDiffs where
--   combineSummaries :: FreeF (Syntax leaf) (Patch (Term leaf Info)) (DiffSummary ()) -> DiffSummary ()
--   combineSummaries (Pure (Insert term)) = TermSummary "insert" () 
--   tearDiffs :: Diff leaf Info -> FreeF (Syntax leaf) (Patch (Term leaf Info)) (Diff leaf Info)
--   tearDiffs = undefined

-- diffSummary' :: Diff leaf Info -> DiffSummary ()
-- -- futu :: Unfoldable t => (a -> Base t (Free (Base t) a)) -> a -> t
-- diffSummary' = futu diffSummary'' where
--   diffSummary'' :: Diff leaf Info -> Prim (DiffSummary a) (Free.Free (Prim (DiffSummary a)) (Diff leaf Info))
--   diffSummary'' diff = case project diff of
--     Pure (Insert term) -> PTermSummary "insert" () undefined
--     Pure (Delete term) -> undefined
--     Pure (Replace t1 t2) -> undefined
--     Free (ann :< Leaf a) -> undefined
-- Syntax Text DiffSummary -> DiffSummary Text
-- If termSummary returns a DiffEntry that just contains the term name, we need to
-- Instead of foldMap we need a histomorphism

termToSummary :: Term leaf Info -> DiffSummary a
termToSummary = Foldable.cata summary where
  summary :: TermF leaf Info f -> DiffSummary a
  summary (info :< Leaf replace) = undefined
  summary (info :< Indexed children) = undefined
  summary (info :< Fixed children) = undefined
  summary (info :< Keyed _) = undefined

maybeFirstCategory :: (Categorizable a) => a -> Maybe Category
maybeFirstCategory term = listToMaybe . toList $ Category.categories term

toCategory :: Info -> a -> DiffSummary a
toCategory info a = case maybeFirstCategory info of
  Just category -> undefined
  Nothing -> undefined
