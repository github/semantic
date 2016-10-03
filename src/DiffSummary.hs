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
import Data.These
import Text.PrettyPrint.Leijen.Text ((<+>), squotes, space, string, Doc, punctuate, pretty, hsep)
import qualified Text.PrettyPrint.Leijen.Text as P
import SourceSpan
import Source

data Annotatable a = Annotatable a | Unannotatable a

annotatable :: (HasField fields Category) => SyntaxTerm leaf fields -> Annotatable (SyntaxTerm leaf fields)
annotatable term = isAnnotatable (category . extract $ term) $ term
  where isAnnotatable = \case
          C.Class -> Annotatable
          C.Method -> Annotatable
          C.Function -> Annotatable
          _ -> Unannotatable

data Identifiable a = Identifiable a | Unidentifiable a

identifiable :: SyntaxTerm leaf fields -> Identifiable (SyntaxTerm leaf fields)
identifiable term = isIdentifiable (unwrap term) $ term
  where isIdentifiable = \case
          S.FunctionCall{} -> Identifiable
          S.MethodCall{} -> Identifiable
          S.Function{} -> Identifiable
          S.Assignment{} -> Identifiable
          S.MathAssignment{} -> Identifiable
          S.VarAssignment{} -> Identifiable
          S.SubscriptAccess{} -> Identifiable
          S.Class{} -> Identifiable
          S.Method{} -> Identifiable
          S.Leaf{} -> Identifiable
          S.DoWhile{} -> Identifiable
          _ -> Unidentifiable

data DiffInfo = LeafInfo { categoryName :: Text, termName :: Text }
 | BranchInfo { branches :: [ DiffInfo ], categoryName :: Text, branchType :: Branch }
 | ErrorInfo { errorSpan :: SourceSpan, termName :: Text }
 deriving (Eq, Show)

data Branch = BIndexed | BFixed | BCommented deriving (Show, Eq, Generic)

data DiffSummary a = DiffSummary {
  patch :: Patch a,
  parentAnnotation :: [(Category, Text)]
} deriving (Eq, Functor, Show, Generic)

-- Returns a list of diff summary texts given two source blobs and a diff.
diffSummaries :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Both SourceBlob -> SyntaxDiff leaf fields -> [Either Text Text]
diffSummaries blobs diff = summaryToTexts =<< diffToDiffSummaries (source <$> blobs) diff

-- Takes a 'DiffSummary' and returns a list of summary texts representing the LeafInfos
-- in that 'DiffSummary'.
summaryToTexts :: DiffSummary DiffInfo -> [Either Text Text]
summaryToTexts DiffSummary{..} = runJoin . fmap (show . (<+> (parentContexts parentAnnotation))) <$> (Join <$> summaries patch)

-- Returns a list of 'DiffSummary' given two source blobs and a diff.
diffToDiffSummaries :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Both (Source Char) -> SyntaxDiff leaf fields -> [DiffSummary DiffInfo]
diffToDiffSummaries sources = para $ \diff ->
  let diff' = free (Prologue.fst <$> diff)
      annotateWithCategory :: [(Diff leaf (Record fields), [DiffSummary DiffInfo])] -> [DiffSummary DiffInfo]
      annotateWithCategory children = maybeToList (prependSummary (Both.snd sources) <$> (afterTerm diff')) <*> (children >>= snd) in
  case diff of
    -- Skip comments and leaves since they don't have any changes
    (Free (_ :< syntax)) -> annotateWithCategory (toList syntax)
    (Pure patch) -> [ DiffSummary (mapPatch (termToDiffInfo beforeSource) (termToDiffInfo afterSource) patch) []]
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
  p@(Replace i1 i2) -> zipWith (\a b -> prefixWithPatch p a <+> "with" <+> determiner i1 <+> b) (toLeafInfos i1) (toLeafInfos i2)
  p@(Insert info) -> prefixWithPatch p <$> toLeafInfos info
  p@(Delete info) -> prefixWithPatch p <$> toLeafInfos info

-- Prefixes a given doc with the type of patch it represents.
prefixWithPatch :: Patch DiffInfo -> Doc -> Doc
prefixWithPatch patch = prefixWithThe (patchToPrefix patch)
  where
    prefixWithThe prefix doc = prefix <+> determiner' patch <+> doc
    patchToPrefix = \case
      (Replace _ _) -> "Replaced"
      (Insert _) -> "Added"
      (Delete _) -> "Deleted"
    determiner' = determiner . these identity identity const . unPatch

-- Optional determiner (e.g. "the") to tie together summary statements.
determiner :: DiffInfo -> Doc
determiner (LeafInfo "number" _) = ""
determiner (LeafInfo "boolean" _) = ""
determiner (LeafInfo "anonymous function" _) = "an"
determiner (BranchInfo bs _ _) = determiner (last bs)
determiner _ = "the"

toLeafInfos :: DiffInfo -> [Doc]
toLeafInfos (LeafInfo "number" termName) = pure (squotes (toDoc termName))
toLeafInfos (LeafInfo "boolean" termName) = pure (squotes (toDoc termName))
toLeafInfos (LeafInfo "anonymous function" termName) = pure (toDoc termName)
toLeafInfos (LeafInfo cName@"string" termName) = pure (toDoc termName <+> toDoc cName)
toLeafInfos LeafInfo{..} = pure (squotes (toDoc termName) <+> toDoc categoryName)
toLeafInfos BranchInfo{..} = toLeafInfos =<< branches
toLeafInfos err@ErrorInfo{} = pure (pretty err)

-- Returns a text representing a specific term given a source and a term.
toTermName :: forall leaf fields. (HasCategory leaf, HasField fields Category, HasField fields Range) => Source Char -> SyntaxTerm leaf fields -> Text
toTermName source term = case unwrap term of
  S.AnonymousFunction maybeParams _ -> "anonymous" <> maybe "" toParams maybeParams <> " function"
    where toParams ps = " (" <> termNameFromSource ps <> ")"
  S.Fixed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  S.Indexed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  Leaf leaf -> toCategoryName leaf
  S.Assignment identifier _ -> toTermName' identifier
  S.Function identifier _ _ -> toTermName' identifier
  S.FunctionCall i args -> toTermName' i <> "(" <> (intercalate ", " (toArgName <$> args)) <> ")"
  S.MemberAccess base property -> case (unwrap base, unwrap property) of
    (S.FunctionCall{}, S.FunctionCall{}) -> toTermName' base <> "()." <> toTermName' property <> "()"
    (S.FunctionCall{}, _) -> toTermName' base <> "()." <> toTermName' property
    (_, S.FunctionCall{}) -> toTermName' base <> "." <> toTermName' property <> "()"
    (_, _) -> toTermName' base <> "." <> toTermName' property
  S.MethodCall targetId methodId methodParams -> toTermName' targetId <> sep <> toTermName' methodId <> "(" <> (intercalate ", " (toArgName <$> methodParams)) <> ")"
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
  S.Object kvs -> "{ " <> intercalate ", " (toTermName' <$> kvs) <> " }"
  S.Pair a _ -> toTermName' a <> ": …"
  S.Return expr -> maybe "empty" toTermName' expr
  S.Error _ _ -> termNameFromSource term
  S.If expr _ _ -> termNameFromSource expr
  S.For clauses _ -> termNameFromChildren term clauses
  S.While expr _ -> toTermName' expr
  S.DoWhile _ expr -> toTermName' expr
  S.Throw expr -> termNameFromSource expr
  S.Constructor expr -> toTermName' expr
  S.Try expr _ _ -> termNameFromSource expr
  S.Array _ -> termNameFromSource term
  S.Class identifier _ _ -> toTermName' identifier
  S.Method identifier _ _ -> toTermName' identifier
  S.Comment a -> toCategoryName a
  S.Commented _ _ -> termNameFromChildren term (toList $ unwrap term)
  where toTermName' = toTermName source
        termNameFromChildren term children = termNameFromRange (unionRangesFrom (range term) (range <$> children))
        termNameFromSource term = termNameFromRange (range term)
        termNameFromRange range = toText $ Source.slice range source
        range = characterRange . extract
        toArgName :: SyntaxTerm leaf fields -> Text
        toArgName arg = case identifiable arg of
                          Identifiable arg -> toTermName' arg
                          Unidentifiable _ -> "…"

parentContexts :: [(Category, Text)] -> Doc
parentContexts contexts = hsep $ go <$> contexts
  where go (c, t) = case c of
          C.Assignment -> "in an" <+> catName <+> "to" <+> termName
          _ -> "in the" <+> termName <+> catName
          where catName = toDoc $ toCategoryName c
                termName = toDoc t

toDoc :: Text -> Doc
toDoc = string . toS

termToDiffInfo :: (HasCategory leaf, HasField fields Category, HasField fields Range) => Source Char -> SyntaxTerm leaf fields -> DiffInfo
termToDiffInfo blob term = case unwrap term of
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BIndexed
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BFixed
  S.AnonymousFunction _ _ -> LeafInfo "anonymous function" (toTermName' term)
  Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (toCategoryName term) BCommented
  S.Error sourceSpan _ -> ErrorInfo sourceSpan (toTermName' term)
  _ -> LeafInfo (toCategoryName term) (toTermName' term)
  where toTermName' = toTermName blob
        termToDiffInfo' = termToDiffInfo blob

-- | Prepends a parentAnnotation to the current DiffSummary instance.
-- | For a DiffSummary without a parentAnnotation, we prepend a parentAnnotation with the first identifiable term.
-- | For a DiffSummary with a parentAnnotation, we prepend the next annotatable term to the extant parentAnnotation.
-- | If a DiffSummary already has a parentAnnotation, and a (grand) parentAnnotation, then we return the summary without modification.
prependSummary :: (HasCategory leaf, HasField fields Range, HasField fields Category) => Source Char -> SyntaxTerm leaf fields -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary source term summary =
  case (identifiable term, annotatable term) of
    (_, Annotatable _) -> summary { parentAnnotation = (category . extract $ term, toTermName source term) : parentAnnotation summary }
    (_, _) -> summary

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
    BooleanOperator -> "boolean operator"
    MathOperator -> "math operator"
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
    C.Ternary -> "ternary expression"
    C.Operator -> "operator"
    Identifier -> "identifier"
    IntegerLiteral -> "integer"
    NumberLiteral -> "number"
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

instance HasField fields Category => HasCategory (SyntaxTerm leaf fields) where
  toCategoryName = toCategoryName . category . extract

instance Arbitrary Branch where
  arbitrary = oneof [ pure BIndexed, pure BFixed ]
  shrink = genericShrink

instance Arbitrary a => Arbitrary (DiffSummary a) where
  arbitrary = DiffSummary <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance P.Pretty DiffInfo where
  pretty LeafInfo{..} = squotes (string $ toSL termName) <+> (string $ toSL categoryName)
  pretty BranchInfo{..} = mconcat $ punctuate (string "," P.<> space) (pretty <$> branches)
  pretty ErrorInfo{..} = squotes (string $ toSL termName) <+> "at" <+> (string . toSL $ displayStartEndPos errorSpan) <+> "in" <+> (string . toSL $ spanName errorSpan)
