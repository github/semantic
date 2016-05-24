{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables, FlexibleInstances, RecordWildCards #-}
module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..)) where

import Prelude hiding (fst, snd)
import Diff
import Info
import Patch
import Term
import Syntax
import Category
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Control.Monad
import Data.Maybe
import Data.List
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.OrderedMap
import qualified Data.Foldable as F
import Data.Text as Text (unpack, Text)

data DiffInfo = DiffInfo { category :: String, value :: Maybe String } deriving (Eq, Show)

class ToTerm a where
  toTerm :: a -> Maybe String

instance HasCategory leaf => ToTerm (Term leaf Info) where
  toTerm term = case runCofree term of
    (_ :< Leaf leaf) -> Just (categoryName leaf)
    (_ :< Keyed children) -> Just (unpack . mconcat $ keys children)
    (_ :< Indexed children) -> Just (categoryName . toCategory . head $ extract <$> children)
    (_ :< Fixed children) -> Just (categoryName . toCategory . head $ extract <$> children)

class HasCategory a where
  categoryName :: a -> String

instance HasCategory String where
  categoryName = id

instance HasCategory Text where
  categoryName = unpack

instance HasCategory Category where
  categoryName category = case category of
    BinaryOperator -> "binary operator"
    DictionaryLiteral -> "dictionary"
    Pair -> "pair"
    FunctionCall -> "function call"
    StringLiteral -> "string"
    IntegerLiteral -> "integer"
    SymbolLiteral -> "symbol"
    ArrayLiteral -> "array"
    (Other s) -> s

instance HasCategory leaf => HasCategory (Term leaf Info) where
  categoryName term = categoryName $ case runCofree term of
    (info :< Leaf _) -> toCategory info
    (info :< Indexed _) -> toCategory info
    (info :< Fixed _) -> toCategory info
    (info :< Keyed _) -> toCategory info


data DiffSummary a = DiffSummary {
  patch :: Patch DiffInfo,
  parentAnnotations :: [DiffInfo]
} deriving (Eq, Functor)

instance Show a => Show (DiffSummary a) where
  show DiffSummary{..} = case patch of
    (Insert termInfo) -> "Added the " ++ "'" ++ fromJust (value termInfo) ++ "' " ++ category termInfo
      ++ maybeParentContext parentAnnotations
    (Delete termInfo) -> "Deleted the " ++ "'" ++ fromJust (value termInfo) ++ "' " ++ category termInfo
      ++ maybeParentContext parentAnnotations
    (Replace t1 t2) -> "Replaced the " ++ "'" ++ fromJust (value t1) ++ "' " ++ category t1
      ++ " with the " ++ "'" ++ fromJust (value t2) ++ "' " ++ category t2
      ++ maybeParentContext parentAnnotations
    where maybeParentContext parentAnnotations = if null parentAnnotations
            then ""
            else " in the " ++ intercalate "/" (category <$> parentAnnotations) ++ " context"

diffSummary :: HasCategory leaf => Diff leaf Info -> [DiffSummary DiffInfo]
diffSummary = cata diffSummary' where
  diffSummary' :: HasCategory leaf => DiffF leaf Info [DiffSummary DiffInfo] -> [DiffSummary DiffInfo]
  diffSummary' (Free (_ :< Leaf _)) = [] -- Skip leaves since they don't have any changes
  diffSummary' (Free (infos :< Indexed children)) = prependSummary (DiffInfo (categoryName . toCategory $ snd infos) Nothing) <$> join children
  diffSummary' (Free (infos :< Fixed children)) = prependSummary (DiffInfo (categoryName . toCategory $ snd infos) Nothing) <$> join children
  diffSummary' (Free (infos :< Keyed children)) = prependSummary (DiffInfo (categoryName . toCategory $ snd infos) Nothing) <$> join (F.toList children)
  diffSummary' (Pure (Insert term)) = [DiffSummary (Insert (DiffInfo (categoryName term) (toTerm term))) []]
  diffSummary' (Pure (Delete term)) = [DiffSummary (Delete (DiffInfo (categoryName term) (toTerm term))) []]
  diffSummary' (Pure (Replace t1 t2)) = [DiffSummary (Replace (DiffInfo (categoryName t1) (toTerm t1)) (DiffInfo (categoryName t2) (toTerm t2))) []]

prependSummary :: DiffInfo -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }

toCategory :: Info -> Category
toCategory info = fromMaybe (Other "Unknown") (maybeFirstCategory info)
