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
import Control.Monad.State hiding (sequence)
import qualified Data.Map as M
import Data.Functor.Identity

instance Unfoldable (DiffSummary a) where
  embed (DiffSummary x y) = (DDiffSummary x y)
  embed (TermSummary s b f) = DTermSummary s b
  embed EmptySummary = DEmptySummary

  apo f a = case f a of
    Cons x (Left xs) -> x : xs
    Cons x (Right b) -> x : apo f b
    Nil -> []


-- * --
--   -- *

data DiffSummary a = BranchSummary [DiffSummary a]
  | TermSummary String a
  | EmptySummary
  deriving (Eq, Show, Functor, Ord)

data instance Prim (DiffSummary a) b = PBranchSummary (Prim [a] b) b | PTermSummary String a b | PEmptySummary

type instance Base (DiffSummary a) = Prim (DiffSummary a)
instance Foldable.Foldable (DiffSummary a) where project = Const
instance Unfoldable (DiffSummary a) where embed = getConst

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
  
-- diffSummary :: Diff leaf Info -> DiffSummary a
-- -- histo :: Foldable t => (Base t (Cofree (Base t) a) -> a) -> t -> a
-- diffSummary = histo diffSummary' . fmap (patchToSummary termToSummary) where
--   --diffSummary' :: DiffF leaf (Cofree.Cofree (DiffF leaf Info) DiffSummary) f -> DiffSummary
--   -- Skip any child that doesn't have any changes (that will always include leaves)
--   diffSummary' :: DiffSummaryF leaf Info DiffSummary (Cofree.Cofree (DiffSummaryF leaf Info DiffSummary) DiffSummary) -> DiffSummary
--   -- Prune leaves
--   diffSummary' (Free (info :< Leaf _)) = undefined
--   -- Return a contextless indexed summary with it's indexed context distributed to its children
--   diffSummary' (Free (_ :< Indexed [])) = undefined
--   diffSummary' (Free (_ :< Indexed ((summary Cofree.:< f):xs))) = summary :: DiffSummary
--   -- Return a contextless fixed diff summary with it's fixed context distributed to its children
--   diffSummary' (Free (_ :< Fixed children)) = undefined
--   diffSummary' (Free (_ :< Keyed children)) = undefined
--   -- Return a contextless diff summary
--   diffSummary' (Pure summary) = summary :: DiffSummary
--   -- (patchSummary termSummary)

diffSummary' :: Diff leaf Info -> DiffSummary a
-- futu :: Unfoldable t => (a -> Base t (Free (Base t) a)) -> a -> t
diffSummary' = futu diffSummary'' where
  diffSummary'' :: (Diff leaf Info) -> Prim (DiffSummary a) (Free.Free (Prim (DiffSummary a)) (Diff leaf Info))
  diffSummary'' diff = case project diff of

-- Syntax Text DiffSummary -> DiffSummary Text
-- If termSummary returns a DiffEntry that just contains the term name, we need to
-- Instead of foldMap we need a histomorphism

termToSummary :: Term leaf Info -> DiffSummary a
termToSummary = Foldable.cata summary where
  summary :: TermF leaf Info f -> DiffSummary a
  summary (info :< Leaf replace) = toCategory info replace
  summary (info :< Indexed children) = _
  summary (info :< Fixed children) = _
  summary (info :< Keyed _) = _

maybeFirstCategory :: (Categorizable a) => a -> Maybe Category
maybeFirstCategory term = listToMaybe . toList $ Category.categories term

toCategory :: Info -> a -> DiffSummary a
toCategory info a = case maybeFirstCategory info of
  Just category -> DTermSummary (show category) a
  Nothing -> DEmptySummary
