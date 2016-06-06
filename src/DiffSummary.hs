{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}
module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..)) where

import Prologue hiding (fst, snd)
import Data.Maybe (fromJust)
import Diff
import Info (Info, category)
import Patch
import Term
import Syntax
import Category
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.OrderedMap
import Data.Text as Text (pack)

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
  parentAnnotations :: [a]
} deriving (Eq, Functor)

instance Show (DiffSummary DiffInfo) where
  showsPrec _ DiffSummary{..} s = (++s) . intercalate "\n" $ case patch of
    (Insert termInfo) -> for (termNames termInfo) $ \name ->
      "Added the " <> "'" <> name <> "' " <> categoryName termInfo
      <> maybeParentContext parentAnnotations
    (Delete termInfo) -> for (termNames termInfo) $ \name ->
      "Deleted the " <> "'" <> name <> "' " <> categoryName termInfo
      <> maybeParentContext parentAnnotations
    (Replace t1 t2) -> for (zip (termNames t1) (termNames t2)) $ \(name1, name2) ->
      "Replaced the " <> "'" ++ name1 ++ "' " ++ categoryName t1
      ++ " with the " ++ "'" ++ name2 ++ "' " ++ categoryName t2
      ++ maybeParentContext parentAnnotations
    where maybeParentContext parentAnnotations = if null parentAnnotations
            then ""
            else " in the " ++ intercalate "/" (categoryName <$> parentAnnotations) ++ " context"

diffSummary :: HasCategory leaf => Diff leaf Info -> [DiffSummary DiffInfo]
diffSummary = cata $ \case
  (Free (_ :< Leaf _)) -> [] -- Skip leaves since they don't have any changes
  (Free (infos :< Indexed children)) -> prependSummary (DiffInfo (toCategoryName . category $ snd infos) []) <$> join children
  (Free (infos :< Fixed children)) -> prependSummary (DiffInfo (toCategoryName . category $ snd infos) []) <$> join children
  (Free (infos :< Keyed children)) -> prependSummary (DiffInfo (toCategoryName . category $ snd infos) []) <$> join (Prologue.toList children)
  (Pure (Insert term)) -> [DiffSummary (Insert (DiffInfo (toCategoryName term) (maybeTermNames term))) []]
  (Pure (Delete term)) -> [DiffSummary (Delete (DiffInfo (toCategoryName term) (maybeTermNames term))) []]
  (Pure (Replace t1 t2)) -> [DiffSummary (Replace (DiffInfo (toCategoryName t1) (maybeTermNames t1)) (DiffInfo (toCategoryName t2) (maybeTermNames t2))) []]

prependSummary :: DiffInfo -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }
