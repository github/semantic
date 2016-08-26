{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}

module DiffSummary (diffSummaries, DiffSummary(..), DiffInfo(..), diffToDiffSummaries, isBranchInfo) where

import Prologue hiding (intercalate)
import Diff
import Patch
import Term
import Info (category, characterRange)
import Range
import Syntax as S
import Category as C
import Data.Functor.Foldable as Foldable
import Data.Functor.Both hiding (fst, snd)
import qualified Data.Functor.Both as Both
import Data.Text as Text (intercalate)
import Test.QuickCheck hiding (Fixed)
import Patch.Arbitrary()
import Data.Record
import Text.PrettyPrint.Leijen.Text ((<+>), squotes, space, string, Doc, punctuate, pretty)
import qualified Text.PrettyPrint.Leijen.Text as P
import SourceSpan
import Source

data Identifiable a = Identifiable a | Unidentifiable a
data DiffInfo = LeafInfo { categoryName :: Text, termName :: Text }
 | BranchInfo { branches :: [ DiffInfo ], categoryName :: Text, branchType :: Branch }
 | ErrorInfo { errorSpan :: SourceSpan, termName :: Text }
 deriving (Eq, Show)

data Branch = BIndexed | BFixed | BCommented deriving (Show, Eq, Generic)

data DiffSummary a = DiffSummary {
  patch :: Patch a,
  parentAnnotation :: Maybe (Category, Text)
} deriving (Eq, Functor, Show, Generic)

-- Returns a list of diff summary texts given two source blobs and a diff.
diffSummaries :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Both SourceBlob -> Diff leaf (Record fields) -> [Either Text Text]
diffSummaries blobs diff = summaryToTexts =<< diffToDiffSummaries (source <$> blobs) diff

-- Takes a 'DiffSummary' and returns a list of summary texts representing the LeafInfos
-- in that 'DiffSummary'.
summaryToTexts :: DiffSummary DiffInfo -> [Either Text Text]
summaryToTexts DiffSummary{..} = runJoin . fmap (show . (P.<> maybeParentContext parentAnnotation)) <$> (Join <$> summaries patch)

-- Returns a list of 'DiffSummary' given two source blobs and a diff.
diffToDiffSummaries :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Both (Source Char) -> Diff leaf (Record fields) -> [DiffSummary DiffInfo]
diffToDiffSummaries sources = para $ \diff ->
  let diff' = free (Prologue.fst <$> diff)
      annotateWithCategory :: [(Diff leaf (Record fields), [DiffSummary DiffInfo])] -> [DiffSummary DiffInfo]
      annotateWithCategory children = maybeToList (prependSummary (Both.snd sources) <$> (afterTerm diff')) <*> (children >>= snd) in
  case diff of
    -- Skip comments and leaves since they don't have any changes
    (Free (_ :< syntax)) -> annotateWithCategory (toList syntax)
    (Pure patch) -> [ DiffSummary (mapPatch (termToDiffInfo beforeSource) (termToDiffInfo afterSource) patch) Nothing ]
  where
    (beforeSource, afterSource) = runJoin sources

-- Returns a list of diff summary 'Docs' prefixed given a 'Patch'.
summaries :: Patch DiffInfo -> [Either Doc Doc]
summaries patch = eitherErrorOrDoc <$> patchToDoc patch
  where eitherErrorOrDoc = if any hasErrorInfo patch then Left else Right

-- Flattens a patch of diff infos into a list of docs, one for every 'LeafInfo'
-- or `ErrorInfo` it contains.
patchToDoc :: Patch DiffInfo -> [Doc]
patchToDoc = \case
  p@(Replace i1 i2) -> zipWith (\a b -> (prefixWithPatch p) a <+> "with the" <+> b) (toLeafInfos i1) (toLeafInfos i2)
  p@(Insert info) -> (prefixWithPatch p) <$> toLeafInfos info
  p@(Delete info) -> (prefixWithPatch p) <$> toLeafInfos info

-- Prefixes a given doc with the type of patch it represents.
prefixWithPatch :: Patch DiffInfo -> Doc -> Doc
prefixWithPatch patch = prefixWithThe (patchToPrefix patch)
  where
    prefixWithThe prefix doc = prefix <+> "the" <+> doc
    patchToPrefix = \case
      (Replace _ _) -> "Replaced"
      (Insert _) -> "Added"
      (Delete _) -> "Deleted"

toLeafInfos :: DiffInfo -> [Doc]
toLeafInfos LeafInfo{..} = pure (squotes (toDoc termName) <+> (toDoc categoryName))
toLeafInfos BranchInfo{..} = toLeafInfos =<< branches
toLeafInfos err@ErrorInfo{} = pure (pretty err)

-- Returns a text representing a specific term given a source and a term.
toTermName :: forall leaf fields. (HasCategory leaf, HasField fields Category, HasField fields Range) => Source Char -> Term leaf (Record fields) -> Text
toTermName source term = case unwrap term of
  S.AnonymousFunction _ _ -> "anonymous"
  S.Fixed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  S.Indexed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  Leaf leaf -> toCategoryName leaf
  S.Assignment identifier value -> case (unwrap identifier, unwrap value) of
    (S.MemberAccess{}, S.AnonymousFunction{..}) -> toTermName' identifier
    (_, _) -> toTermName' identifier <> toTermName' value
  S.Function identifier _ _ -> toTermName' identifier
  S.FunctionCall i args -> toTermName' i <> "(" <> (intercalate ", " (toArgName <$> args)) <> ")"
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
  S.Operator _ -> termNameFromSource term
  S.Object kvs -> "{" <> intercalate ", " (toTermName' <$> kvs) <> "}"
  S.Pair a b -> toTermName' a <> ": " <> toTermName' b
  S.Return expr -> maybe "empty" toTermName' expr
  S.Error _ _ -> termNameFromSource term
  S.If expr _ _ -> termNameFromSource expr
  S.For _ _ -> termNameFromChildren term
  S.While expr _ -> toTermName' expr
  S.DoWhile _ expr -> toTermName' expr
  S.Throw expr -> termNameFromSource expr
  S.Constructor expr -> toTermName' expr
  S.Try expr _ _ -> termNameFromSource expr
  S.Array _ -> termNameFromSource term
  S.Class identifier _ _ -> toTermName' identifier
  S.Method identifier _ _ -> toTermName' identifier
  S.Comment a -> toCategoryName a
  S.Commented _ _ -> termNameFromChildren term
  where toTermName' = toTermName source
        termNameFromChildren term = termNameFromRange (unionRangesFrom (range term) (range <$> toList (unwrap term)))
        termNameFromSource term = termNameFromRange (range term)
        termNameFromRange range = toText $ Source.slice range source
        range = characterRange . extract
        toArgName :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Term leaf (Record fields) -> Text
        toArgName arg = case unwrap arg of
          S.AnonymousFunction _ _ -> "..."
          _ -> toTermName' arg

maybeParentContext :: Maybe (Category, Text) -> Doc
maybeParentContext = maybe "" (\annotation ->
  space <> "in the" <+> (toDoc $ snd annotation) <+> toDoc (toCategoryName $ fst annotation))

toDoc :: Text -> Doc
toDoc = string . toS

termToDiffInfo :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Source Char -> Term leaf (Record fields) -> DiffInfo
termToDiffInfo blob term = case unwrap term of
  Leaf _ -> LeafInfo (toCategoryName term) (toTermName' term)
  S.AnonymousFunction _ _ -> LeafInfo (toCategoryName term) ("anonymous")
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BIndexed
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BFixed
  S.Ternary ternaryCondition _ -> LeafInfo (toCategoryName term) (toTermName' ternaryCondition)
  S.Function identifier _ _ -> LeafInfo (toCategoryName term) (toTermName' identifier)
  S.Assignment identifier _ -> LeafInfo (toCategoryName term) (toTermName' identifier)
  S.MathAssignment identifier _ -> LeafInfo (toCategoryName term) (toTermName' identifier)
  -- Currently we cannot express the operator for an operator production from TreeSitter. Eventually we should be able to
  -- use the term name of the operator identifier when we have that production value. Until then, I'm using a placeholder value
  -- to indicate where that value should be when constructing DiffInfos.
  Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (toCategoryName term) BCommented
  S.Error sourceSpan _ -> ErrorInfo sourceSpan (toTermName' term)
  _ -> LeafInfo (toCategoryName term) (toTermName' term)
  where toTermName' = toTermName blob
        termToDiffInfo' = termToDiffInfo blob

prependSummary :: (HasCategory leaf, HasField fields Range, HasField fields Category) => Source Char -> Term leaf (Record fields) -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary source term summary = if (isNothing $ parentAnnotation summary) && hasIdentifier term
  then summary { parentAnnotation = Just (category $ extract term, toTermName source term) }
  else summary
  where hasIdentifier term = case unwrap term of
          S.FunctionCall{} -> True
          S.Function _ _ _ -> True
          S.Assignment{} -> True
          S.MathAssignment{} -> True
          S.MemberAccess{} -> True
          S.MethodCall{} -> True
          S.VarAssignment{} -> True
          S.SubscriptAccess{} -> True
          S.Class{} -> True
          S.Method{} -> True
          _ -> False

isBranchInfo :: DiffInfo -> Bool
isBranchInfo info = case info of
  (BranchInfo _ _ _) -> True
  _ -> False

hasErrorInfo :: DiffInfo -> Bool
hasErrorInfo info = case info of
  (ErrorInfo _ _) -> True
  (BranchInfo branches _ _) -> any hasErrorInfo branches
  _ -> False

-- The user-facing category name of 'a'.
class HasCategory a where
  toCategoryName :: a -> Text

-- Instances

instance HasCategory Text where
  toCategoryName = identity

instance HasCategory Category where
  toCategoryName = \case
    ArrayLiteral -> "array"
    BinaryOperator -> "binary operator"
    BitwiseOperator -> "bitwise operator"
    RelationalOperator -> "relational operator"
    Boolean -> "boolean"
    DictionaryLiteral -> "dictionary"
    C.Comment -> "comment"
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
    C.Throw -> "throw statement"
    C.Constructor -> "constructor"
    C.Catch -> "catch statement"
    C.Try -> "try statement"
    C.Finally -> "finally statement"
    C.Class -> "class"
    C.Method -> "method"
    C.If -> "if statement"
    C.CommaOperator -> "comma operator"
    C.Empty -> "empty statement"

instance (HasCategory leaf, HasField fields Category) => HasCategory (Term leaf (Record fields)) where
  toCategoryName = toCategoryName . category . extract

instance Arbitrary Branch where
  arbitrary = oneof [ pure BIndexed, pure BFixed ]
  shrink = genericShrink

instance (Eq a, Arbitrary a) => Arbitrary (DiffSummary a) where
  arbitrary = DiffSummary <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance P.Pretty DiffInfo where
  pretty LeafInfo{..} = squotes (string $ toSL termName) <+> (string $ toSL categoryName)
  pretty BranchInfo{..} = mconcat $ punctuate (string "," <> space) (pretty <$> branches)
  pretty ErrorInfo{..} = squotes (string $ toSL termName) <+> "at" <+> (string . toSL $ displayStartEndPos errorSpan) <+> "in" <+> (string . toSL $ spanName errorSpan)
