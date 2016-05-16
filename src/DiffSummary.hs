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

diffSummary :: Diff leaf Info -> [DiffSummary ()]
diffSummary = cata diffSummary' where
  diffSummary' :: DiffF leaf Info [DiffSummary ()] -> [DiffSummary ()]
  diffSummary' (Free (info :< Leaf _)) = [] -- Skip leaves since they don't have any changes
  diffSummary' (Free (_ :< Indexed children)) = prependSummary () <$> join children
  diffSummary' (Free (_ :< Fixed children)) = prependSummary () <$> join children
  diffSummary' (Free (_ :< Keyed children)) = prependSummary () <$> join (F.toList children)
  diffSummary' (Pure (Insert term)) = [TermSummary "insert" () []]
  diffSummary' (Pure (Delete term)) = [TermSummary "delete" () []]
  diffSummary' (Pure (Replace t1 t2)) = [TermSummary "replace" () []]

prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }

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
