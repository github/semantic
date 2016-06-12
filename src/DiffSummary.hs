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

data DiffInfo = DiffInfo { categoryName :: Text, termName :: Text } deriving (Eq, Show)

toTermName :: HasCategory leaf => Term leaf Info -> Text
toTermName term = case runCofree term of
  (_ :< Leaf leaf) -> toCategoryName leaf
  (_ :< Keyed children) -> mconcat $ keys children
  (_ :< Indexed children) -> fromMaybe "EmptyIndexedNode" $ (toCategoryName . category) . extract <$> head children
  (_ :< Fixed children) -> fromMaybe "EmptyFixedNode" $ (toCategoryName . category) . extract <$> head children
  (_ :< Syntax.FunctionCall i _) -> toTermName i
  (_ :< Syntax.Function identifier _ _) -> (maybe "anonymous" toTermName identifier)
  (_ :< Syntax.Assignment identifier value) -> toTermName identifier <> toTermName value
  (_ :< Syntax.MemberAccess base property) -> case (unwrap base, unwrap property) of
    (Syntax.FunctionCall{}, Syntax.FunctionCall{}) -> toTermName base <> "()." <> toTermName property <> "()"
    (Syntax.FunctionCall{}, _) -> toTermName base <> "()." <> toTermName property
    (_, Syntax.FunctionCall{}) -> toTermName base <> "." <> toTermName property <> "()"
    (_, _) -> toTermName base <> "." <> toTermName property

class HasCategory a where
  toCategoryName :: a -> Text

instance HasCategory Text where
  toCategoryName = identity

instance HasCategory Info where
  toCategoryName = toCategoryName . category

instance HasCategory Category where
  toCategoryName = \case
    Program -> "top level"
    Error -> "error"
    BinaryOperator -> "binary operator"
    DictionaryLiteral -> "dictionary"
    Pair -> "pair"
    Category.FunctionCall -> "function call"
    Category.MethodCall -> "method call"
    StringLiteral -> "string"
    IntegerLiteral -> "integer"
    SymbolLiteral -> "symbol"
    ArrayLiteral -> "array"
    Category.Function -> "function"
    Identifier -> "identifier"
    Params -> "params"
    ExpressionStatements -> "expression statements"
    Category.Assignment -> "assignment"
    Category.MemberAccess -> "member access"
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
  (Free (infos :< Syntax.FunctionCall identifier children)) -> prependSummary (category $ snd infos) <$> join (Prologue.toList (identifier : children))
  (Free (infos :< Syntax.Function id ps body)) -> prependSummary (category $ snd infos) <$> (fromMaybe [] id) <> (fromMaybe [] ps) <> body
  (Free (infos :< Syntax.Assignment id value)) -> prependSummary (category $ snd infos) <$> id <> value
  (Free (infos :< Syntax.MemberAccess base property)) -> prependSummary (category $ snd infos) <$> base <> property
  (Free (infos :< Syntax.MethodCall targetId methodId ps)) -> prependSummary (category $ snd infos) <$> targetId <> methodId <> ps
  (Pure (Insert term)) -> (\info -> DiffSummary (Insert info) []) <$> termToDiffInfo term
  (Pure (Delete term)) -> (\info -> DiffSummary (Delete info) []) <$> termToDiffInfo term
  (Pure (Replace t1 t2)) -> (\(info1, info2) -> DiffSummary (Replace info1 info2) []) <$> zip (termToDiffInfo t1) (termToDiffInfo t2)

termToDiffInfo :: HasCategory leaf => Term leaf Info -> [DiffInfo]
termToDiffInfo term = case runCofree term of
  (_ :< Leaf _) -> [ DiffInfo (toCategoryName term) (toTermName term) ]
  (_ :< Indexed children) -> join $ termToDiffInfo <$> children
  (_ :< Fixed children) -> join $ termToDiffInfo <$> children
  (_ :< Keyed children) -> join $ termToDiffInfo <$> Prologue.toList children
  (info :< Syntax.FunctionCall identifier _) -> [ DiffInfo (toCategoryName info) (toTermName identifier) ]
  (info :< Syntax.Function identifier _ _) -> [ DiffInfo (toCategoryName info) (maybe "anonymous" toTermName identifier) ]
  (info :< Syntax.Assignment identifier value) -> [ DiffInfo (toCategoryName info) (toTermName identifier) ]
  memberAccess@(info :< Syntax.MemberAccess{}) -> [ DiffInfo (toCategoryName info) (toTermName $ cofree memberAccess) ]
  (info :< Syntax.MethodCall _ methodId _) -> [ DiffInfo (toCategoryName info) (toTermName methodId) ]

prependSummary :: Category -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }
