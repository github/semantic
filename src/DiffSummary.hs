{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}

module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..), annotatedSummaries) where

import Prologue hiding (snd, intercalate)
import Diff
import Patch
import Term
import Info (category, characterRange)
import Range
import Syntax as S
import Category as C
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.Text as Text (intercalate)
import Test.QuickCheck hiding (Fixed)
import Patch.Arbitrary()
import Data.Record
import Text.PrettyPrint.Leijen.Text ((<+>), squotes, space, string, Doc, punctuate, pretty)
import qualified Text.PrettyPrint.Leijen.Text as P
import SourceSpan
import Source

data DiffInfo = LeafInfo { categoryName :: Text, termName :: Text }
 | BranchInfo { branches :: [ DiffInfo ], categoryName :: Text, branchType :: Branch }
 | ErrorInfo { errorSpan :: SourceSpan, categoryName :: Text }
 deriving (Eq, Show)

toTermName :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Source Char -> Term leaf (Record fields) -> Text
toTermName source term = case unwrap term of
  S.Fixed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  S.Indexed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  Leaf leaf -> toCategoryName leaf
  S.Assignment identifier value -> toTermName' identifier <> toTermName' value
  S.Function identifier _ _ -> (maybe "anonymous" toTermName' identifier)
  S.FunctionCall i _ -> toTermName' i
  S.MemberAccess base property -> case (unwrap base, unwrap property) of
    (S.FunctionCall{}, S.FunctionCall{}) -> toTermName' base <> "()." <> toTermName' property <> "()"
    (S.FunctionCall{}, _) -> toTermName' base <> "()." <> toTermName' property
    (_, S.FunctionCall{}) -> toTermName' base <> "." <> toTermName' property <> "()"
    (_, _) -> toTermName' base <> "." <> toTermName' property
  S.MethodCall targetId methodId _ -> toTermName' targetId <> sep <> toTermName' methodId <> "()"
    where sep = case unwrap targetId of
            S.FunctionCall{} -> "()."
            _ -> "."
  S.SubscriptAccess base element -> case (unwrap base, unwrap element) of
    (S.FunctionCall{}, S.FunctionCall{}) -> toTermName' base <> "()." <> toTermName' element <> "()"
    (S.FunctionCall{}, _) -> toTermName' base <> "()." <> toTermName' element
    (_, S.FunctionCall{}) -> toTermName' base <> "[" <> toTermName' element <> "()" <> "]"
    (_, _) -> toTermName' base <> "[" <> toTermName' element <> "]"
  S.VarAssignment varId _ -> toTermName' varId
  S.VarDecl decl -> toTermName' decl
  -- TODO: We should remove Args from Syntax since I don't think we should ever
  -- evaluate Args as a single toTermName Text - joshvera
  S.Args args -> mconcat $ toTermName' <$> args
  -- TODO: We should remove Case from Syntax since I don't think we should ever
  -- evaluate Case as a single toTermName Text - joshvera
  S.Case expr _ -> toTermName' expr
  S.Switch expr _ -> toTermName' expr
  S.Ternary expr _ -> toTermName' expr
  S.MathAssignment id _ -> toTermName' id
  S.Operator exprs -> termNameFromChildren term exprs
  S.Object kvs -> "{" <> intercalate ", " (toTermName' <$> kvs) <> "}"
  S.Pair a b -> toTermName' a <> ": " <> toTermName' b
  S.Return expr -> maybe "empty" toTermName' expr
  S.For exprs _ -> termNameFromChildren term exprs
  S.While expr _ -> toTermName' expr
  S.DoWhile _ expr -> toTermName' expr
  Comment a -> toCategoryName a
  where toTermName' = toTermName source
        termNameFromChildren term cs = toText $ Source.slice (unionRangesFrom (range term) (range <$> cs)) source
        range term = (characterRange $ extract term)

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
    C.Error -> "error"
    ExpressionStatements -> "expression statements"
    C.Assignment -> "assignment"
    C.Function -> "function"
    C.FunctionCall -> "function call"
    C.MemberAccess -> "member access"
    C.MethodCall -> "method call"
    C.Args -> "arguments"
    C.VarAssignment -> "var assignment"
    C.VarDecl -> "variable"
    C.Switch -> "switch statement"
    C.Case -> "case statement"
    C.SubscriptAccess -> "subscript access"
    C.MathAssignment -> "math assignment"
    C.Ternary -> "ternary"
    C.Operator -> "operator"
    Identifier -> "identifier"
    IntegerLiteral -> "integer"
    Other s -> s
    C.Pair -> "pair"
    Params -> "params"
    Program -> "top level"
    Regex -> "regex"
    StringLiteral -> "string"
    SymbolLiteral -> "symbol"
    TemplateString -> "template string"
    C.For -> "for statement"
    C.While -> "while statement"
    C.DoWhile -> "do/while statement"
    C.Object -> "object"
    C.Return -> "return statement"

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
  pretty ErrorInfo{..} = "syntax error at" <+> (string . toSL $ displayStartEndPos errorSpan) <+> "in" <+> (string . toSL $ spanName errorSpan)

annotatedSummaries :: DiffSummary DiffInfo -> [Text]
annotatedSummaries DiffSummary{..} = show . (P.<> maybeParentContext parentAnnotations) <$> summaries patch

summaries :: Patch DiffInfo -> [P.Doc]
summaries (Insert info) = (("Added" <+> "the") <+>) <$> toLeafInfos info
summaries (Delete info) = (("Deleted" <+> "the") <+>) <$> toLeafInfos info
summaries (Replace i1 i2) = zipWith (\a b -> "Replaced" <+> "the" <+> a <+> "with the" <+> b) (toLeafInfos i1) (toLeafInfos i2)

toLeafInfos :: DiffInfo -> [Doc]
toLeafInfos LeafInfo{..} = pure $ squotes (toDoc termName) <+> (toDoc categoryName)
toLeafInfos BranchInfo{..} = pretty <$> branches
toLeafInfos err@ErrorInfo{} = pure $ pretty err

maybeParentContext :: [Category] -> Doc
maybeParentContext annotations = if null annotations
  then ""
  else space <> "in the" <+> (toDoc . intercalate "/" $ toCategoryName <$> annotations) <+> "context"
toDoc :: Text -> Doc
toDoc = string . toS

diffSummary :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Both (Source Char) -> Diff leaf (Record fields) -> [DiffSummary DiffInfo]
diffSummary sources = cata $ \case
  -- Skip comments and leaves since they don't have any changes
  (Free (_ :< Leaf _)) -> []
  Free (_ :< (S.Comment _)) -> []
  (Free (infos :< S.Indexed children)) -> annotateWithCategory infos <$> join children
  (Free (infos :< S.Fixed children)) -> annotateWithCategory infos <$> join children
  (Free (infos :< S.FunctionCall identifier children)) -> annotateWithCategory infos <$> join (Prologue.toList (identifier : children))
  (Free (infos :< S.Function id ps body)) -> annotateWithCategory infos <$> (fromMaybe [] id) <> (fromMaybe [] ps) <> body
  (Free (infos :< S.Assignment id value)) -> annotateWithCategory infos <$> id <> value
  (Free (infos :< S.MemberAccess base property)) -> annotateWithCategory infos <$> base <> property
  (Free (infos :< S.SubscriptAccess base property)) -> annotateWithCategory infos <$> base <> property
  (Free (infos :< S.MethodCall targetId methodId ps)) -> annotateWithCategory infos <$> targetId <> methodId <> ps
  (Free (infos :< S.VarAssignment varId value)) -> annotateWithCategory infos <$> varId <> value
  (Free (infos :< S.VarDecl decl)) -> annotateWithCategory infos <$> decl
  (Free (infos :< S.Args args)) -> annotateWithCategory infos <$> join args
  (Free (infos :< S.Switch expr cases)) -> annotateWithCategory infos <$> expr <> join cases
  (Free (infos :< S.Case expr body)) -> annotateWithCategory infos <$> expr <> body
  Free (infos :< (S.Ternary expr cases)) -> annotateWithCategory infos <$> expr <> join cases
  Free (infos :< (S.MathAssignment id value)) -> annotateWithCategory infos <$> id <> value
  Free (infos :< (S.Operator syntaxes)) -> annotateWithCategory infos <$> join syntaxes
  Free (infos :< (S.Object kvs)) -> annotateWithCategory infos <$> join kvs
  Free (infos :< (S.Return expr)) -> annotateWithCategory infos <$> fromMaybe [] expr
  Free (infos :< (S.Pair a b)) -> annotateWithCategory infos <$> a <> b
  Free (infos :< (S.Commented cs leaf)) -> annotateWithCategory infos <$> join cs <> fromMaybe [] leaf
  Free (infos :< (S.Error _ children)) -> annotateWithCategory infos <$> join children
  (Free (infos :< S.For exprs body)) -> annotateWithCategory infos <$> join exprs <> body
  (Free (infos :< S.While expr body)) -> annotateWithCategory infos <$> expr <> body
  (Free (infos :< S.DoWhile expr body)) -> annotateWithCategory infos <$> expr <> body
  (Pure (Insert term)) -> [ DiffSummary (Insert $ termToDiffInfo afterSource term) [] ]
  (Pure (Delete term)) -> [ DiffSummary (Delete $ termToDiffInfo beforeSource term) [] ]
  (Pure (Replace t1 t2)) -> [ DiffSummary (Replace (termToDiffInfo beforeSource t1) (termToDiffInfo afterSource t2)) [] ]
  where
    (beforeSource, afterSource) = runJoin sources
    annotateWithCategory infos = prependSummary (category $ snd infos)


termToDiffInfo :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Source Char -> Term leaf (Record fields) -> DiffInfo
termToDiffInfo blob term = case unwrap term of
  Leaf _ -> LeafInfo (toCategoryName term) (toTermName' term)
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BIndexed
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BFixed
  S.FunctionCall identifier _ -> LeafInfo (toCategoryName term) (toTermName' identifier)
  S.Ternary ternaryCondition _ -> LeafInfo (toCategoryName term) (toTermName' ternaryCondition)
  S.Function identifier _ _ -> LeafInfo (toCategoryName term) (maybe "anonymous" toTermName' identifier)
  S.Assignment identifier _ -> LeafInfo (toCategoryName term) (toTermName' identifier)
  S.MathAssignment identifier _ -> LeafInfo (toCategoryName term) (toTermName' identifier)
  -- Currently we cannot express the operator for an operator production from TreeSitter. Eventually we should be able to
  -- use the term name of the operator identifier when we have that production value. Until then, I'm using a placeholder value
  -- to indicate where that value should be when constructing DiffInfos.
  Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (toCategoryName term) BCommented
  S.Error sourceSpan _ -> ErrorInfo sourceSpan (toCategoryName term)
  _ -> LeafInfo (toCategoryName term) (toTermName' term)
  where toTermName' = toTermName blob
        termToDiffInfo' = termToDiffInfo blob

prependSummary :: Category -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }
