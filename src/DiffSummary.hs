{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}
module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..)) where

import Prelude hiding (fst, snd)
import Diff
import Info
import Patch
import Syntax
import Category
import Data.Maybe (listToMaybe)
import Data.Set (toList)
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Control.Monad
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable as F

data DiffInfo = DiffInfo deriving (Eq)

data DiffSummary a = DiffSummary {
  description :: String,
  patch :: Patch DiffInfo,
  parentAnnotations :: [a]
} deriving (Eq, Functor)

instance Show a => Show (DiffSummary a) where
  show diffSummary = case patch diffSummary of
    (Replace _ _) -> "Replaced "
    (Insert term) -> "Added "
    (Delete term) -> "Deleted "

diffSummary :: Diff leaf Info -> [DiffSummary ()]
diffSummary = cata diffSummary' where
  diffSummary' :: DiffF leaf Info [DiffSummary ()] -> [DiffSummary ()]
  diffSummary' (Free (_ :< Leaf _)) = [] -- Skip leaves since they don't have any changes
  diffSummary' (Free (_ :< Indexed children)) = prependSummary () <$> join children
  diffSummary' (Free (_ :< Fixed children)) = prependSummary () <$> join children
  diffSummary' (Free (_ :< Keyed children)) = prependSummary () <$> join (F.toList children)
  diffSummary' (Pure (Insert _)) = [DiffSummary "insert" (Insert DiffInfo) []]
  diffSummary' (Pure (Delete _)) = [DiffSummary "delete" (Delete DiffInfo) []]
  diffSummary' (Pure (Replace _ _)) = [DiffSummary "delete" (Replace DiffInfo DiffInfo) []]

prependSummary :: a -> DiffSummary a -> DiffSummary a
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }

maybeFirstCategory :: (Categorizable a) => a -> Maybe Category
maybeFirstCategory term = listToMaybe . toList $ Category.categories term

toCategory :: Info -> a -> DiffSummary a
toCategory info a = case maybeFirstCategory info of
  Just category -> undefined
  Nothing -> undefined
