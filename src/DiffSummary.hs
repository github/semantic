{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables, DeriveAnyClass #-}

module DiffSummary (diffSummaries, DiffSummary(..), DiffInfo(..), diffToDiffSummaries, isBranchInfo, isErrorSummary, JSONSummary(..)) where

import Prologue hiding (intercalate)
import Diff
import Patch
import Term
import Info (category, characterRange)
import Range
import Syntax as S
import Category as C
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
import Data.Aeson as A

data Annotatable a = Annotatable a | Unannotatable a

annotatable :: SyntaxTerm leaf fields -> Annotatable (SyntaxTerm leaf fields)
annotatable term = isAnnotatable (unwrap term) term
  where isAnnotatable = \case
          S.Class{} -> Annotatable
          S.Method{} -> Annotatable
          S.Function{} -> Annotatable
          S.Module{} -> Annotatable
          _ -> Unannotatable

data Identifiable a = Identifiable a | Unidentifiable a

identifiable :: SyntaxTerm leaf fields -> Identifiable (SyntaxTerm leaf fields)
identifiable term = isIdentifiable (unwrap term) term
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
          S.Import{} -> Identifiable
          S.Export{} -> Identifiable
          _ -> Unidentifiable

data JSONSummary summary span = JSONSummary { summary :: summary, span :: span }
                 | ErrorSummary { summary :: summary, span :: span }
                 deriving (Generic, Eq, Show)

instance (ToJSON summary, ToJSON span) => ToJSON (JSONSummary summary span) where
  toJSON JSONSummary{..} = object [ "summary" .= summary, "span" .= span ]
  toJSON ErrorSummary{..} = object [ "summary" .= summary, "span" .= span ]

isErrorSummary :: JSONSummary summary span -> Bool
isErrorSummary ErrorSummary{} = True
isErrorSummary _ = False

data DiffInfo = LeafInfo { categoryName :: Text, termName :: Text, sourceSpan :: SourceSpan }
 | BranchInfo { branches :: [ DiffInfo ], categoryName :: Text, branchType :: Branch }
 | ErrorInfo { errorSpan :: SourceSpan, termName :: Text }
 deriving (Eq, Show)

data Branch = BIndexed | BFixed | BCommented deriving (Show, Eq, Generic)

data DiffSummary a = DiffSummary {
  patch :: Patch a,
  parentAnnotation :: [Either (Category, Text) (Category, Text)]
} deriving (Eq, Functor, Show, Generic)

-- Returns a list of diff summary texts given two source blobs and a diff.
diffSummaries :: (HasCategory leaf, HasField fields Category, HasField fields Range, HasField fields SourceSpan) =>
                  Both SourceBlob ->
                  SyntaxDiff leaf fields ->
                  [JSONSummary Text SourceSpans]
diffSummaries blobs diff = summaryToTexts =<< diffToDiffSummaries (source <$> blobs) diff

-- Takes a 'DiffSummary' and returns a list of summary texts representing the LeafInfos
-- in that 'DiffSummary'.
summaryToTexts :: DiffSummary DiffInfo -> [JSONSummary Text SourceSpans]
summaryToTexts DiffSummary{..} = (\jsonSummary ->
  jsonSummary { summary = show $ summary jsonSummary <+> parentContexts parentAnnotation }) <$> summaries patch

-- Returns a list of 'DiffSummary' given two source blobs and a diff.
diffToDiffSummaries :: (HasCategory leaf, HasField fields Category, HasField fields Range, HasField fields SourceSpan) => Both (Source Char) -> SyntaxDiff leaf fields -> [DiffSummary DiffInfo]
diffToDiffSummaries sources = para $ \diff ->
  let
    diff' = free (Prologue.fst <$> diff)
    annotateWithCategory :: [DiffSummary DiffInfo] -> [DiffSummary DiffInfo]
    annotateWithCategory children = case (beforeTerm diff', afterTerm diff') of
      (_, Just diff'') -> appendSummary (Both.snd sources) diff'' <$> children
      (Just diff'', _) -> appendSummary (Both.fst sources) diff'' <$> children
      (Nothing, Nothing) -> []
  in case diff of
    -- Skip comments and leaves since they don't have any changes
    (Free (_ :< syntax)) -> annotateWithCategory (toList syntax >>= snd)
    (Pure patch) -> [ DiffSummary (mapPatch (termToDiffInfo beforeSource) (termToDiffInfo afterSource) patch) [] ]
  where
    (beforeSource, afterSource) = runJoin sources

-- Flattens a patch of diff infos into a list of docs, one for every 'LeafInfo' or `ErrorInfo` it contains.
summaries :: Patch DiffInfo -> [JSONSummary Doc SourceSpans]
summaries = \case
  p@(Replace i1 i2) -> zipWith (\a b ->
    JSONSummary
     {
      summary = summary (prefixWithPatch p This a) <+> "with" <+> determiner i1 <+> summary b
    , span = SourceSpans $ These (span a) (span b)
    }) (toLeafInfos i1) (toLeafInfos i2)
  p@(Insert info) -> prefixWithPatch p That <$> toLeafInfos info
  p@(Delete info) -> prefixWithPatch p This <$> toLeafInfos info

-- Prefixes a given doc with the type of patch it represents.
prefixWithPatch :: Patch DiffInfo -> (SourceSpan -> These SourceSpan SourceSpan) -> JSONSummary Doc SourceSpan -> JSONSummary Doc SourceSpans
prefixWithPatch patch constructor = prefixWithThe (patchToPrefix patch)
  where
    prefixWithThe prefix jsonSummary = jsonSummary
      {
        summary = prefix <+> determiner' patch <+> summary jsonSummary
      , span = SourceSpans $ constructor (span jsonSummary)
      }
    patchToPrefix = \case
      (Replace _ _) -> "Replaced"
      (Insert _) -> "Added"
      (Delete _) -> "Deleted"
    determiner' = determiner . these identity identity const . unPatch

-- Optional determiner (e.g. "the") to tie together summary statements.
determiner :: DiffInfo -> Doc
determiner (LeafInfo "number" _ _) = ""
determiner (LeafInfo "boolean" _ _) = ""
determiner (LeafInfo "anonymous function" _ _) = "an"
determiner (BranchInfo bs _ _) = determiner (last bs)
determiner _ = "the"

toLeafInfos :: DiffInfo -> [JSONSummary Doc SourceSpan]
toLeafInfos err@ErrorInfo{..} = pure $ ErrorSummary (pretty err) errorSpan
toLeafInfos BranchInfo{..} = branches >>= toLeafInfos
toLeafInfos leaf = pure . flip JSONSummary (sourceSpan leaf) $ case leaf of
  (LeafInfo "number" termName _) -> squotes $ toDoc termName
  (LeafInfo "boolean" termName _) -> squotes $ toDoc termName
  (LeafInfo "anonymous function" termName _) -> squotes (toDoc termName) <+> string "function"
  (LeafInfo cName@"string" termName _) -> toDoc termName <+> toDoc cName
  (LeafInfo cName@"export statement" termName _) -> P.enclose (string "{ ") (string " }") (toDoc termName) <+> toDoc cName
  LeafInfo{..} -> squotes (toDoc termName) <+> toDoc categoryName
  node -> panic $ "Expected a leaf info but got a: " <> show node

-- Returns a text representing a specific term given a source and a term.
toTermName :: forall leaf fields. (HasCategory leaf, HasField fields Category, HasField fields Range) => Source Char -> SyntaxTerm leaf fields -> Text
toTermName source term = case unwrap term of
  S.AnonymousFunction params _ -> "anonymous" <> paramsToArgNames params
  S.Fixed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  S.Indexed children -> fromMaybe "branch" $ (toCategoryName . category) . extract <$> head children
  Leaf leaf -> toCategoryName leaf
  S.Assignment identifier _ -> toTermName' identifier
  S.Function identifier _ _ ->
    toTermName' identifier
  S.FunctionCall i args -> case unwrap i of
    S.AnonymousFunction params _ ->
      -- Omit a function call's arguments if it's arguments match the underlying
      -- anonymous function's arguments.
      if (category . extract <$> args) == (category . extract <$> params)
      then toTermName' i
      else "(" <> toTermName' i <> ")" <> paramsToArgNames args
    _ -> toTermName' i <> paramsToArgNames args
  S.MemberAccess base property -> case (unwrap base, unwrap property) of
    (S.FunctionCall{}, S.FunctionCall{}) -> toTermName' base <> "()." <> toTermName' property <> "()"
    (S.FunctionCall{}, _) -> toTermName' base <> "()." <> toTermName' property
    (_, S.FunctionCall{}) -> toTermName' base <> "." <> toTermName' property <> "()"
    (_, _) -> toTermName' base <> "." <> toTermName' property
  S.MethodCall targetId methodId methodParams -> toTermName' targetId <> sep <> toTermName' methodId <> paramsToArgNames methodParams
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
  S.Error _ -> termNameFromSource term
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
  S.Module identifier _ -> toTermName' identifier
  S.Import identifier _ -> toTermName' identifier
  S.Export Nothing expr -> intercalate ", " $ termNameFromSource <$> expr
  S.Export (Just identifier) [] -> toTermName' identifier
  S.Export (Just identifier) expr -> (intercalate ", " $ termNameFromSource <$> expr) <> " from " <> toTermName' identifier
  where toTermName' = toTermName source
        termNameFromChildren term children = termNameFromRange (unionRangesFrom (range term) (range <$> children))
        termNameFromSource term = termNameFromRange (range term)
        termNameFromRange range = toText $ Source.slice range source
        range = characterRange . extract
        paramsToArgNames params = "(" <> intercalate ", " (toArgName <$> params) <> ")"
        toArgName :: SyntaxTerm leaf fields -> Text
        toArgName arg = case identifiable arg of
                          Identifiable arg -> toTermName' arg
                          Unidentifiable _ -> "…"

parentContexts :: [Either (Category, Text) (Category, Text)] -> Doc
parentContexts contexts = hsep $ either identifiableDoc annotatableDoc <$> contexts
  where
    identifiableDoc (c, t) = case c of
      C.Assignment -> "in an" <+> catName c <+> "to" <+> termName t
      _ -> "in the" <+> termName t <+> catName c
    annotatableDoc (c, t) = "of the" <+> squotes (termName t) <+> catName c
    catName = toDoc . toCategoryName
    termName = toDoc

toDoc :: Text -> Doc
toDoc = string . toS

termToDiffInfo :: (HasCategory leaf, HasField fields Category, HasField fields Range, HasField fields SourceSpan) => Source Char -> SyntaxTerm leaf fields -> DiffInfo
termToDiffInfo blob term = case unwrap term of
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BIndexed
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (toCategoryName term) BFixed
  S.AnonymousFunction _ _ -> LeafInfo "anonymous function" (toTermName' term) (getField $ extract term)
  Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (toCategoryName term) BCommented
  S.Error _ -> ErrorInfo (getField $ extract term) (toTermName' term)
  _ -> LeafInfo (toCategoryName term) (toTermName' term) (getField $ extract term)
  where toTermName' = toTermName blob
        termToDiffInfo' = termToDiffInfo blob

-- | Append a parentAnnotation to the current DiffSummary instance.
-- | For a DiffSummary without a parentAnnotation, we append a parentAnnotation with the first identifiable term.
-- | For a DiffSummary with a parentAnnotation, we append the next annotatable term to the extant parentAnnotation.
-- | If a DiffSummary already has a parentAnnotation, and a (grand) parentAnnotation, then we return the summary without modification.
appendSummary :: (HasCategory leaf, HasField fields Range, HasField fields Category) => Source Char -> SyntaxTerm leaf fields -> DiffSummary DiffInfo -> DiffSummary DiffInfo
appendSummary source term summary =
  case (parentAnnotation summary, identifiable term, annotatable term) of
    ([], Identifiable _, _) -> appendParentAnnotation Left
    ([_], _, Annotatable _) -> appendParentAnnotation Right
    (_, _, _) -> summary
  where
    appendParentAnnotation constructor = summary
      { parentAnnotation = parentAnnotation summary <> [ constructor (category (extract term), toTermName source term) ] }

isBranchInfo :: DiffInfo -> Bool
isBranchInfo info = case info of
  BranchInfo{} -> True
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
    C.Module -> "module statement"
    C.Import -> "import statement"
    C.Export -> "export statement"

instance HasField fields Category => HasCategory (SyntaxTerm leaf fields) where
  toCategoryName = toCategoryName . category . extract

instance Arbitrary Branch where
  arbitrary = oneof [ pure BIndexed, pure BFixed ]
  shrink = genericShrink

instance Arbitrary a => Arbitrary (DiffSummary a) where
  arbitrary = DiffSummary <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance P.Pretty DiffInfo where
  pretty LeafInfo{..} = squotes (string $ toSL termName) <+> string (toSL categoryName)
  pretty BranchInfo{..} = mconcat $ punctuate (string "," P.<> space) (pretty <$> branches)
  pretty ErrorInfo{..} = squotes (string $ toSL termName) <+> "at" <+> (string . toSL $ displayStartEndPos errorSpan)
