{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}

module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..)) where

import Prologue hiding (fst, snd, intercalate)
import Diff
import Info (Info, category)
import Patch
import Term
import Syntax
import Category
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.OrderedMap
import Data.Text as Text (intercalate, unpack)

data DiffInfo = DiffInfo { categoryName :: Text, termNames :: [Text] } deriving (Eq, Show)

maybeTermNames :: HasCategory leaf => Term leaf Info -> [Text]
maybeTermNames term = case runCofree term of
  (_ :< Leaf leaf) -> pure $ toCategoryName leaf
  (_ :< Keyed children) -> keys children
  (_ :< Indexed children) -> toCategoryName . category <$> extract <$> children
  (_ :< Fixed children) -> toCategoryName . category <$> extract <$> children

class HasCategory a where
  toCategoryName :: a -> Text

instance HasCategory Text where
  toCategoryName = identity

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

instance HasCategory leaf => HasCategory (Term leaf Info) where
  toCategoryName = toCategoryName . category . extract

data DiffSummary a = DiffSummary {
  patch :: Patch a,
  parentAnnotations :: [Category]
} deriving (Eq, Functor)

instance Show (DiffSummary DiffInfo) where
  showsPrec _ DiffSummary{..} s = (++s) . unpack $ case patch of
    (Insert diffInfo) -> "Added the " <> "'" <> termName diffInfo <> "' " <> categoryName diffInfo <> maybeParentContext parentAnnotations
    (Delete diffInfo) -> "Deleted the " <> "'" <> termName diffInfo <> "' " <> categoryName diffInfo <> maybeParentContext parentAnnotations
    (Replace t1 t2) ->
      "Replaced the " <> "'" <> termName t1 <> "' " <> categoryName t1
      <> " with the " <> "'" <> termName t2 <> "' " <> categoryName t2
      <> maybeParentContext parentAnnotations
    where maybeParentContext parentAnnotations = if null parentAnnotations
            then ""
            else " in the " <> intercalate "/" (toCategoryName <$> parentAnnotations) <> " context"

diffSummary :: HasCategory leaf => Diff leaf Info -> [DiffSummary DiffInfo]
diffSummary = cata $ \case
  (Free (_ :< Leaf _)) -> [] -- Skip leaves since they don't have any changes
  (Free (infos :< Indexed children)) -> prependSummary (category $ snd infos) <$> join children
  (Free (infos :< Fixed children)) -> prependSummary (category $ snd infos) <$> join children
  (Free (infos :< Keyed children)) -> prependSummary (category $ snd infos) <$> join (Prologue.toList children)
  (Pure (Insert term)) -> [DiffSummary (Insert (DiffInfo (toCategoryName term) (maybeTermName term))) []]
  (Pure (Delete term)) -> [DiffSummary (Delete (DiffInfo (toCategoryName term) (maybeTermName term))) []]
  (Pure (Replace t1 t2)) -> [DiffSummary (Replace (DiffInfo (toCategoryName t1) (maybeTermName t1)) (DiffInfo (toCategoryName t2) (maybeTermName t2))) []]

prependSummary :: Category -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }
