{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}

module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..), annotatedSummaries) where

import Prologue hiding (snd, intercalate)
import Diff
import Patch
import Term
import Info (category)
import Syntax
import Category
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.Text as Text (intercalate)
import Test.QuickCheck hiding (Fixed)
import Patch.Arbitrary()
import Data.Record
import Text.PrettyPrint.Leijen.Text ((<+>), squotes, space, string, Doc, punctuate, pretty)
import qualified Text.PrettyPrint.Leijen.Text as P

data DiffInfo = LeafInfo { categoryName :: Text, termName :: Text }
 | BranchInfo { branches :: [ DiffInfo ], categoryName :: Text, branchType :: Branch }
 deriving (Eq, Show)

toTermName :: (HasCategory leaf, HasField fields Category) => Term leaf (Record fields) -> Text
toTermName term = case unwrap term of
  Syntax.Fixed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  Syntax.Indexed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  Leaf leaf -> toCategoryName leaf
  Syntax.Assignment identifier value -> toTermName identifier <> toTermName value
  Syntax.Function identifier _ _ -> (maybe "anonymous" toTermName identifier)
  Syntax.FunctionCall i _ -> toTermName i
  Syntax.MemberAccess base property -> case (unwrap base, unwrap property) of
    (Syntax.FunctionCall{}, Syntax.FunctionCall{}) -> toTermName base <> "()." <> toTermName property <> "()"
    (Syntax.FunctionCall{}, _) -> toTermName base <> "()." <> toTermName property
    (_, Syntax.FunctionCall{}) -> toTermName base <> "." <> toTermName property <> "()"
    (_, _) -> toTermName base <> "." <> toTermName property
  Syntax.MethodCall targetId methodId _ -> toTermName targetId <> sep <> toTermName methodId <> "()"
    where sep = case unwrap targetId of
            Syntax.FunctionCall{} -> "()."
            _ -> "."
  Syntax.SubscriptAccess base element -> case (unwrap base, unwrap element) of
    (Syntax.FunctionCall{}, Syntax.FunctionCall{}) -> toTermName base <> "()." <> toTermName element <> "()"
    (Syntax.FunctionCall{}, _) -> toTermName base <> "()." <> toTermName element
    (_, Syntax.FunctionCall{}) -> toTermName base <> "[" <> toTermName element <> "()" <> "]"
    (_, _) -> toTermName base <> "[" <> toTermName element <> "]"
  Syntax.VarAssignment varId _ -> toTermName varId
  Syntax.VarDecl decl -> toTermName decl
  -- TODO: We should remove Args from Syntax since I don't think we should ever
  -- evaluate Args as a single toTermName Text - joshvera
  Syntax.Args args -> mconcat $ toTermName <$> args
  -- TODO: We should remove Case from Syntax since I don't think we should ever
  -- evaluate Case as a single toTermName Text - joshvera
  Syntax.Case expr _ -> toTermName expr
  Syntax.Switch expr _ -> toTermName expr
  Syntax.For expr value _ -> toTermName expr <> " in " <> toTermName value
  Syntax.Ternary expr _ -> toTermName expr
  Syntax.MathAssignment id _ -> toTermName id
  Syntax.Operator syntaxes -> mconcat $ toTermName <$> syntaxes
  Syntax.Object kvs -> "{" <> intercalate ", " (toTermName <$> kvs) <> "}"
  Syntax.Pair a b -> toTermName a <> ": " <> toTermName b
  Comment a -> toCategoryName a

class HasCategory a where
  toCategoryName :: a -> Text

instance HasCategory Text where
  toCategoryName = identity

instance HasCategory Category where
  toCategoryName = \case
    ArrayLiteral -> "array"
    BinaryOperator -> "binary operator"
    Boolean -> "boolean"
    DictionaryLiteral -> "dictionary"
    Error -> "error"
    ExpressionStatements -> "expression statements"
    Category.Assignment -> "assignment"
    Category.Function -> "function"
    Category.FunctionCall -> "function call"
    Category.MemberAccess -> "member access"
    Category.MethodCall -> "method call"
    Category.Args -> "arguments"
    Category.VarAssignment -> "var assignment"
    Category.VarDecl -> "variable"
    Category.Switch -> "switch statement"
    Category.Case -> "case statement"
    Category.SubscriptAccess -> "subscript access"
    Category.MathAssignment -> "math assignment"
    Category.Ternary -> "ternary"
    Category.Operator -> "operator"
    Identifier -> "identifier"
    IntegerLiteral -> "integer"
    Other s -> s
    Category.Pair -> "pair"
    Params -> "params"
    Program -> "top level"
    Regex -> "regex"
    StringLiteral -> "string"
    SymbolLiteral -> "symbol"
    TemplateString -> "template string"
    Category.For -> "for statement"
    Category.Object -> "object"

instance (HasCategory leaf, HasField fields Category) => HasCategory (Term leaf (Record fields)) where
  toCategoryName = toCategoryName . category . extract

data Branch = BIndexed | BFixed | BCommented deriving (Show, Eq, Generic)
instance Arbitrary Branch where
  arbitrary = oneof [ pure BIndexed, pure BFixed ]
  shrink = genericShrink

data DiffSummary a = DiffSummary {
  patch :: Patch a,
  parentAnnotations :: [Category]
} deriving (Eq, Functor, Show, Generic)

instance (Eq a, Arbitrary a) => Arbitrary (DiffSummary a) where
  arbitrary = DiffSummary <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance P.Pretty DiffInfo where
  pretty LeafInfo{..} = squotes (string $ toSL termName) <+> (string $ toSL categoryName)
  pretty BranchInfo{..} = mconcat $ punctuate (string "," <> space) (pretty <$> branches)

annotatedSummaries :: DiffSummary DiffInfo -> [Text]
annotatedSummaries DiffSummary{..} = show . (P.<> maybeParentContext parentAnnotations) <$> summaries patch

summaries :: Patch DiffInfo -> [P.Doc]
summaries (Insert info) = (("Added" <+> "the") <+>) <$> toLeafInfos info
summaries (Delete info) = (("Deleted" <+> "the") <+>) <$> toLeafInfos info
summaries (Replace i1 i2) = zipWith (\a b -> "Replaced" <+> "the" <+> a <+> "with the" <+> b) (toLeafInfos i1) (toLeafInfos i2)

toLeafInfos :: DiffInfo -> [Doc]
toLeafInfos LeafInfo{..} = [ squotes (toDoc termName) <+> (toDoc categoryName) ]
toLeafInfos BranchInfo{..} = pretty <$> branches

maybeParentContext :: [Category] -> Doc
maybeParentContext annotations = if null annotations
  then ""
  else space <> "in the" <+> (toDoc . intercalate "/" $ toCategoryName <$> annotations) <+> "context"
toDoc :: Text -> Doc
toDoc = string . toS

diffSummary :: (HasCategory leaf, HasField fields Category) => Diff leaf (Record fields) -> [DiffSummary DiffInfo]
diffSummary = cata $ \case
  -- Skip comments and leaves since they don't have any changes
  (Free (_ :< Leaf _)) -> []
  Free (_ :< (Syntax.Comment _)) -> []
  (Free (infos :< Syntax.Indexed children)) -> prependSummary (category $ snd infos) <$> join children
  (Free (infos :< Syntax.Fixed children)) -> prependSummary (category $ snd infos) <$> join children
  (Free (infos :< Syntax.FunctionCall identifier children)) -> prependSummary (category $ snd infos) <$> join (Prologue.toList (identifier : children))
  (Free (infos :< Syntax.Function id ps body)) -> prependSummary (category $ snd infos) <$> (fromMaybe [] id) <> (fromMaybe [] ps) <> body
  (Free (infos :< Syntax.Assignment id value)) -> prependSummary (category $ snd infos) <$> id <> value
  (Free (infos :< Syntax.MemberAccess base property)) -> prependSummary (category $ snd infos) <$> base <> property
  (Free (infos :< Syntax.SubscriptAccess base property)) -> prependSummary (category $ snd infos) <$> base <> property
  (Free (infos :< Syntax.MethodCall targetId methodId ps)) -> prependSummary (category $ snd infos) <$> targetId <> methodId <> ps
  (Free (infos :< Syntax.VarAssignment varId value)) -> prependSummary (category $ snd infos) <$> varId <> value
  (Free (infos :< Syntax.VarDecl decl)) -> prependSummary (category $ snd infos) <$> decl
  (Free (infos :< Syntax.Args args)) -> prependSummary (category $ snd infos) <$> join args
  (Free (infos :< Syntax.Switch expr cases)) -> prependSummary (category $ snd infos) <$> expr <> join cases
  (Free (infos :< Syntax.Case expr body)) -> prependSummary (category $ snd infos) <$> expr <> body
  Free (infos :< (Syntax.Ternary expr cases)) -> prependSummary (category $ snd infos) <$> expr <> join cases
  Free (infos :< (Syntax.MathAssignment id value)) -> prependSummary (category $ snd infos) <$> id <> value
  Free (infos :< (Syntax.Operator syntaxes)) -> prependSummary (category $ snd infos) <$> join syntaxes
  Free (infos :< (Syntax.Object kvs)) -> prependSummary (category $ snd infos) <$> join kvs
  Free (infos :< (Syntax.Pair a b)) -> prependSummary (category $ snd infos) <$> a <> b
  Free (infos :< (Syntax.Commented cs leaf)) -> prependSummary (category $ snd infos) <$> join cs <> fromMaybe [] leaf
  (Pure (Insert term)) -> [ DiffSummary (Insert $ termToDiffInfo term) [] ]
  (Pure (Delete term)) -> [ DiffSummary (Delete $ termToDiffInfo term) [] ]
  (Pure (Replace t1 t2)) -> [ DiffSummary (Replace (termToDiffInfo t1) (termToDiffInfo t2)) [] ]

termToDiffInfo :: (HasCategory leaf, HasField fields Category) => Term leaf (Record fields) -> DiffInfo
termToDiffInfo term = case unwrap term of
  Leaf _ -> LeafInfo (toCategoryName term) (toTermName term)
  Syntax.Indexed children -> BranchInfo (termToDiffInfo <$> children) (toCategoryName term) BIndexed
  Syntax.Fixed children -> BranchInfo (termToDiffInfo <$> children) (toCategoryName term) BFixed
  Syntax.FunctionCall identifier _ -> LeafInfo (toCategoryName term) (toTermName identifier)
  Syntax.Ternary ternaryCondition _ -> LeafInfo (toCategoryName term) (toTermName ternaryCondition)
  Syntax.Function identifier _ _ -> LeafInfo (toCategoryName term) (maybe "anonymous" toTermName identifier)
  Syntax.Assignment identifier _ -> LeafInfo (toCategoryName term) (toTermName identifier)
  Syntax.MathAssignment identifier _ -> LeafInfo (toCategoryName term) (toTermName identifier)
  -- Currently we cannot express the operator for an operator production from TreeSitter. Eventually we should be able to
  -- use the term name of the operator identifier when we have that production value. Until then, I'm using a placeholder value
  -- to indicate where that value should be when constructing DiffInfos.
  Syntax.Operator _ -> LeafInfo (toCategoryName term) "x"
  Commented cs leaf -> BranchInfo (termToDiffInfo <$> cs <> maybeToList leaf) (toCategoryName term) BCommented
  _ ->  LeafInfo (toCategoryName term) (toTermName term)

prependSummary :: Category -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }
