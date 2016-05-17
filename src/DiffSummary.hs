{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}
module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..)) where

import Prelude hiding (fst, snd)
import Diff
import Info
import Patch
import Term
import Syntax
import Category
import Data.Maybe (fromMaybe)
import Data.Set (toList)
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Control.Monad
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable as F

data DiffInfo = DiffInfo { termName :: String } deriving (Eq)

data DiffSummary a = DiffSummary {
  patch :: Patch DiffInfo,
  parentAnnotations :: [a]
} deriving (Eq, Functor)

instance Show a => Show (DiffSummary a) where
  show diffSummary = case patch diffSummary of
    (Replace _ _) -> "Replaced "
    (Insert termInfo) -> "Added " ++ show (termName termInfo)
    (Delete termInfo) -> "Deleted "

diffSummary :: Show leaf => Diff leaf Info -> [DiffSummary ()]
diffSummary = cata diffSummary' where
  diffSummary' :: Show leaf => DiffF leaf Info [DiffSummary ()] -> [DiffSummary ()]
  diffSummary' (Free (_ :< Leaf _)) = [] -- Skip leaves since they don't have any changes
  diffSummary' (Free (_ :< Indexed children)) = prependSummary () <$> join children
  diffSummary' (Free (_ :< Fixed children)) = prependSummary () <$> join children
  diffSummary' (Free (_ :< Keyed children)) = prependSummary () <$> join (F.toList children)
  diffSummary' (Pure (Insert term)) = [DiffSummary (Insert (DiffInfo (toTermName term))) []]
  diffSummary' (Pure (Delete term)) = [DiffSummary (Delete (DiffInfo (toTermName term))) []]
  diffSummary' (Pure (Replace t1 t2)) = [DiffSummary (Replace (DiffInfo (toTermName t1)) (DiffInfo (toTermName t2))) []]

prependSummary :: a -> DiffSummary a -> DiffSummary a
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }

toTermName :: Show leaf => Term leaf Info -> String
toTermName term = case runCofree term of
  (_ :< (Leaf leaf)) -> show leaf
  (info :< Indexed _) -> show $ toCategory info
  (info :< Fixed _) -> show $ toCategory info
  (info :< Keyed _) -> show $ toCategory info

toCategory :: Info -> Category
toCategory info = fromMaybe (Other "Unknown") (maybeFirstCategory info)
