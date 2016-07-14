{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}
module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..)) where

import Prologue hiding (fst, snd)
import Data.String
import Data.Maybe (fromJust)
import Diff
import Info (Info, category)
import Patch
import Term
import Syntax
import Category
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.Record
import Data.Text as Text (unpack)

data DiffInfo = DiffInfo { categoryName :: String, termName :: Maybe String } deriving (Eq, Show)

maybeTermName :: (HasCategory leaf, HasField fields Category) => Term leaf (Record fields) -> Maybe String
maybeTermName term = case runCofree term of
  (_ :< Leaf leaf) -> Just (toCategoryName leaf)
  (_ :< Indexed children) -> toCategoryName . category <$> head (extract <$> children)
  (_ :< Fixed children) -> toCategoryName . category <$> head (extract <$> children)

class HasCategory a where
  toCategoryName :: a -> String

instance HasCategory String where
  toCategoryName = identity

instance HasCategory Text where
  toCategoryName = unpack

instance HasCategory Category where
  toCategoryName category = case category of
    Program -> "top level"
    Error -> "error"
    BinaryOperator -> "binary operator"
    DictionaryLiteral -> "dictionary"
    Pair -> "pair"
    FunctionCall -> "function call"
    StringLiteral -> "string"
    IntegerLiteral -> "integer"
    SymbolLiteral -> "symbol"
    ArrayLiteral -> "array"
    Other s -> s

instance (HasCategory leaf, HasField fields Category) => HasCategory (Term leaf (Record fields)) where
  toCategoryName = toCategoryName . category . extract

data DiffSummary a = DiffSummary {
  patch :: Patch a,
  parentAnnotations :: [a]
} deriving (Eq, Functor)

instance Show (DiffSummary DiffInfo) where
  showsPrec _ DiffSummary{..} s = (++s) $ case patch of
    (Insert termInfo) -> "Added the " ++ "'" ++ fromJust (termName termInfo) ++ "' " ++ categoryName termInfo
      ++ maybeParentContext parentAnnotations
    (Delete termInfo) -> "Deleted the " ++ "'" ++ fromJust (termName termInfo) ++ "' " ++ categoryName termInfo
      ++ maybeParentContext parentAnnotations
    (Replace t1 t2) -> "Replaced the " ++ "'" ++ fromJust (termName t1) ++ "' " ++ categoryName t1
      ++ " with the " ++ "'" ++ fromJust (termName t2) ++ "' " ++ categoryName t2
      ++ maybeParentContext parentAnnotations
    where maybeParentContext parentAnnotations = if null parentAnnotations
            then ""
            else " in the " ++ intercalate "/" (categoryName <$> parentAnnotations) ++ " context"

diffSummary :: HasCategory leaf => Diff leaf Info -> [DiffSummary DiffInfo]
diffSummary = cata diffSummary' where
  diffSummary' :: HasCategory leaf => Base (Diff leaf Info) [DiffSummary DiffInfo] -> [DiffSummary DiffInfo]
  diffSummary' (Free (_ :< Leaf _)) = [] -- Skip leaves since they don't have any changes
  diffSummary' (Free (infos :< Indexed children)) = prependSummary (DiffInfo (toCategoryName . category $ snd infos) Nothing) <$> join children
  diffSummary' (Free (infos :< Fixed children)) = prependSummary (DiffInfo (toCategoryName . category $ snd infos) Nothing) <$> join children
  diffSummary' (Pure (Insert term)) = [DiffSummary (Insert (DiffInfo (toCategoryName term) (maybeTermName term))) []]
  diffSummary' (Pure (Delete term)) = [DiffSummary (Delete (DiffInfo (toCategoryName term) (maybeTermName term))) []]
  diffSummary' (Pure (Replace t1 t2)) = [DiffSummary (Replace (DiffInfo (toCategoryName t1) (maybeTermName t1)) (DiffInfo (toCategoryName t2) (maybeTermName t2))) []]

prependSummary :: DiffInfo -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }
