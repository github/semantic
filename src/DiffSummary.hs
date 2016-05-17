{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables, FlexibleInstances #-}
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
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Control.Monad
import Data.List
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import qualified Data.Foldable as F

data DiffInfo = DiffInfo { name :: String } deriving (Eq, Show)

class IsTerm a where
  termName :: a -> String

instance IsTerm DiffInfo where
  termName = name

instance IsTerm String where
  termName = id

instance IsTerm Category where
  termName category = case category of
    BinaryOperator -> "binary operator"
    DictionaryLiteral -> "dictionary literal"
    Pair -> "pair"
    FunctionCall -> "function call"
    StringLiteral -> "string literal"
    IntegerLiteral -> "integer literal"
    SymbolLiteral -> "symbol literal"
    ArrayLiteral -> "array literal"
    (Other s) -> s

data DiffSummary a = DiffSummary {
  patch :: Patch DiffInfo,
  parentAnnotations :: [DiffInfo]
} deriving (Eq, Functor)

instance Show a => Show (DiffSummary a) where
  show diffSummary = case patch diffSummary of
    (Replace _ _) -> "Replaced "
    (Insert termInfo) -> "Added the " ++ "'" ++ termName termInfo ++ "' "
      ++ "to the " ++ intercalate "#" (termName <$> parentAnnotations diffSummary) ++ " context"
    (Delete termInfo) -> "Deleted "

diffSummary :: Show leaf => Diff leaf Info -> [DiffSummary DiffInfo]
diffSummary = cata diffSummary' where
  diffSummary' :: Show leaf => DiffF leaf Info [DiffSummary DiffInfo] -> [DiffSummary DiffInfo]
  diffSummary' (Free (_ :< Leaf _)) = [] -- Skip leaves since they don't have any changes
  diffSummary' (Free (infos :< Indexed children)) = prependSummary (DiffInfo . termName . toCategory $ snd infos) <$> join children
  diffSummary' (Free (infos :< Fixed children)) = prependSummary (DiffInfo . termName . toCategory $ snd infos) <$> join children
  diffSummary' (Free (infos :< Keyed children)) = prependSummary (DiffInfo . termName . toCategory $ snd infos) <$> join (F.toList children)
  diffSummary' (Pure (Insert term)) = [DiffSummary (Insert (DiffInfo (toTermName term))) []]
  diffSummary' (Pure (Delete term)) = [DiffSummary (Delete (DiffInfo (toTermName term))) []]
  diffSummary' (Pure (Replace t1 t2)) = [DiffSummary (Replace (DiffInfo (toTermName t1)) (DiffInfo (toTermName t2))) []]

prependSummary :: DiffInfo -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }

toTermName :: Show leaf => Term leaf Info -> String
toTermName term = termName $ case runCofree term of
  (info :< Leaf _) -> toCategory info
  (info :< Indexed _) -> toCategory info
  (info :< Fixed _) -> toCategory info
  (info :< Keyed _) -> toCategory info

toCategory :: Info -> Category
toCategory info = fromMaybe (Other "Unknown") (maybeFirstCategory info)
