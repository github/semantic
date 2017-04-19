{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- Disabling deprecation warnings due to pattern match against RescueModifier.
module Renderer.Summary (Summaries(..), summary, diffSummaries, DiffSummary(..), DiffInfo(..), diffToDiffSummaries, isBranchInfo, isErrorSummary, JSONSummary(..)) where

import Prologue
import Diff
import Patch
import Term
import Info (HasDefaultFields, category, byteRange)
import Range
import Syntax as S
import Category as C
import Data.Functor.Both hiding (fst, snd)
import qualified Data.Functor.Both as Both
import Data.Functor.Listable
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as Text
import Data.Text.Listable
import Data.Record
import Data.These
import Text.PrettyPrint.Leijen.Text ((<+>), squotes, space, string, Doc, punctuate, pretty, hsep)
import qualified Text.PrettyPrint.Leijen.Text as P
import Data.Aeson
import SourceSpan
import Source hiding (null)
import qualified Data.Map as Map
import qualified Data.List as List

data Summaries = Summaries { changes, errors :: !(Map Text [Value]) }
  deriving Show

instance Monoid Summaries where
  mempty = Summaries mempty mempty
  mappend (Summaries c1 e1) (Summaries c2 e2) = Summaries (Map.unionWith (<>) c1 c2) (Map.unionWith (<>) e1 e2)

instance (StringConv Summaries ByteString) where
  strConv _ = toS . (<> "\n") . encode

instance ToJSON Summaries where
  toJSON Summaries{..} = object [ "changes" .= changes, "errors" .= errors ]

data Annotatable a = Annotatable a | Unannotatable a

annotatable :: SyntaxTerm leaf fields -> Annotatable (SyntaxTerm leaf fields)
annotatable term = isAnnotatable (unwrap term) term
  where isAnnotatable syntax = case syntax of
          S.Class{} -> Annotatable
          S.Method{} -> Annotatable
          S.Function{} -> Annotatable
          S.Module{} -> Annotatable
          S.Namespace{} -> Annotatable
          S.Interface{} -> Annotatable
          _ -> Unannotatable

data Identifiable a = Identifiable a | Unidentifiable a

identifiable :: SyntaxTerm leaf fields -> Identifiable (SyntaxTerm leaf fields)
identifiable term = isIdentifiable (unwrap term) term
  where isIdentifiable syntax = case syntax of
          S.FunctionCall{} -> Identifiable
          S.MethodCall{} -> Identifiable
          S.Function{} -> Identifiable
          S.Assignment{} -> Identifiable
          S.OperatorAssignment{} -> Identifiable
          S.VarAssignment{} -> Identifiable
          S.SubscriptAccess{} -> Identifiable
          S.Module{} -> Identifiable
          S.Namespace{} -> Identifiable
          S.Interface{} -> Identifiable
          S.Class{} -> Identifiable
          S.Method{} -> Identifiable
          S.Leaf{} -> Identifiable
          S.DoWhile{} -> Identifiable
          S.Import{} -> Identifiable
          S.Export{} -> Identifiable
          S.Ternary{} -> Identifiable
          S.If{} -> Identifiable
          S.Try{} -> Identifiable
          S.Switch{} -> Identifiable
          S.Rescue{} -> Identifiable
          S.Pair{} -> Identifiable
          S.Array ty _ -> maybe Unidentifiable (const Identifiable) ty
          S.Object ty _ -> maybe Unidentifiable (const Identifiable) ty
          S.BlockStatement{} -> Identifiable
          S.TypeDecl{} -> Identifiable
          S.Ty{} -> Identifiable
          _ -> Unidentifiable

data JSONSummary info span = JSONSummary { info :: info, span :: span }
                 | ErrorSummary { info :: info, span :: span }
                 deriving (Generic, Eq, Show)

instance (ToJSON info, ToJSON span) => ToJSON (JSONSummary info span) where
  toJSON JSONSummary{..} = object [ "summary" .= info, "span" .= span ]
  toJSON ErrorSummary{..} = object [ "summary" .= info, "span" .= span ]

isErrorSummary :: JSONSummary info span -> Bool
isErrorSummary ErrorSummary{} = True
isErrorSummary _ = False

data DiffInfo = LeafInfo { leafCategory :: Category, termName :: Text, sourceSpan :: SourceSpan }
 | BranchInfo { branches :: [ DiffInfo ], branchCategory :: Category, branchType :: Branch }
 | ErrorInfo { errorSpan :: SourceSpan, termName :: Text }
 | HideInfo -- Hide/Strip from summary output entirely.
 deriving (Eq, Show)

data Branch = BIndexed | BFixed | BCommented | BIf deriving (Show, Eq, Generic)

data DiffSummary a = DiffSummary {
  diffSummaryPatch :: Patch a,
  parentAnnotation :: [Either (Category, Text) (Category, Text)]
} deriving (Eq, Functor, Show, Generic)

summary :: HasDefaultFields fields => Both SourceBlob -> Diff (Syntax Text) (Record fields) -> Summaries
summary blobs diff = Summaries changes errors
  where
    changes = if null changes' then mempty else Map.singleton summaryKey (toJSON <$> changes')
    errors = if null errors' then mempty else Map.singleton summaryKey (toJSON <$> errors')
    (errors', changes') = List.partition isErrorSummary summaries
    summaryKey = toSummaryKey (path <$> blobs)
    summaries = diffSummaries blobs diff

    -- Returns a key representing the filename. If the filenames are different,
    -- return 'before -> after'.
    toSummaryKey :: Both FilePath -> Text
    toSummaryKey = runBothWith $ \before after ->
      toS $ case (before, after) of
        ("", after) -> after
        (before, "") -> before
        (before, after) | before == after -> after
        (before, after) | not (null before) && not (null after) -> before <> " -> " <> after
        (_, _) -> mempty

-- Returns a list of diff summary texts given two source blobs and a diff.
diffSummaries :: (StringConv leaf Text, HasDefaultFields fields) => Both SourceBlob -> SyntaxDiff leaf fields -> [JSONSummary Text SourceSpans]
diffSummaries blobs diff = summaryToTexts =<< diffToDiffSummaries (source <$> blobs) diff

-- Takes a 'DiffSummary DiffInfo' and returns a list of JSON Summaries whose text summaries represent the LeafInfo summaries of the 'DiffSummary'.
summaryToTexts :: DiffSummary DiffInfo -> [JSONSummary Text SourceSpans]
summaryToTexts DiffSummary{..} = appendParentContexts <$> jsonDocSummaries diffSummaryPatch
  where appendParentContexts jsonSummary =
          jsonSummary { info = show $ info jsonSummary <+> parentContexts parentAnnotation }

-- Returns a list of 'DiffSummary' given two source blobs and a diff.
diffToDiffSummaries :: (StringConv leaf Text, HasDefaultFields fields) => Both Source -> SyntaxDiff leaf fields -> [DiffSummary DiffInfo]
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
jsonDocSummaries :: Patch DiffInfo -> [JSONSummary Doc SourceSpans]
jsonDocSummaries patch = case patch of
  Replace i1 i2 -> zipWith (\a b ->
    JSONSummary
     {
      info = info (prefixWithPatch patch This a) <+> "with" <+> info b
    , span = SourceSpans $ These (span a) (span b)
    }) (toLeafInfos i1) (toLeafInfos i2)
  Insert info -> prefixWithPatch patch That <$> toLeafInfos info
  Delete info -> prefixWithPatch patch This <$> toLeafInfos info

-- Prefixes a given doc with the type of patch it represents.
prefixWithPatch :: Patch DiffInfo -> (SourceSpan -> These SourceSpan SourceSpan) -> JSONSummary Doc SourceSpan -> JSONSummary Doc SourceSpans
prefixWithPatch patch constructor = prefixWithThe (patchToPrefix patch)
  where
    prefixWithThe prefix jsonSummary = jsonSummary
      {
        info = prefix <+> info jsonSummary
      , span = SourceSpans $ constructor (span jsonSummary)
      }
    patchToPrefix patch = case patch of
      (Replace _ _) -> "Replaced"
      (Insert _) -> "Added"
      (Delete _) -> "Deleted"

toLeafInfos :: DiffInfo -> [JSONSummary Doc SourceSpan]
toLeafInfos err@ErrorInfo{..} = pure $ ErrorSummary (pretty err) errorSpan
toLeafInfos BranchInfo{..} = branches >>= toLeafInfos
toLeafInfos HideInfo = []
toLeafInfos LeafInfo{..} = pure $ JSONSummary (summary leafCategory termName) sourceSpan
  where
    summary :: Category -> Text -> Doc
    summary category termName = case category of
      C.NumberLiteral -> squotes $ toDoc termName
      C.IntegerLiteral -> squotes $ toDoc termName
      C.Boolean -> squotes $ toDoc termName
      C.StringLiteral -> termAndCategoryName
      C.Export -> termAndCategoryName
      C.Import -> termAndCategoryName
      C.Subshell -> termAndCategoryName
      C.AnonymousFunction -> "an" <+> toDoc termName <+> "function"
      C.Begin -> categoryName'
      C.Select -> categoryName'
      C.Else -> categoryName'
      C.Ensure -> categoryName'
      C.Break -> categoryName'
      C.Continue -> categoryName'
      C.BeginBlock -> categoryName'
      C.EndBlock -> categoryName'
      C.Yield | Text.null termName -> categoryName'
      C.Return | Text.null termName -> categoryName'
      C.Switch | Text.null termName -> categoryName'
      _ -> "the" <+> squotes (toDoc termName) <+> toDoc categoryName
      where
        termAndCategoryName = "the" <+> toDoc termName <+> toDoc categoryName
        categoryName = toCategoryName category
        categoryName' = case categoryName of
          name | startsWithVowel name -> "an" <+> toDoc name
               | otherwise -> "a" <+> toDoc name
        startsWithVowel text = getAny $ foldMap (Any . flip Text.isPrefixOf text) vowels
        vowels = Text.singleton <$> ("aeiouAEIOU" :: [Char])

-- Returns a text representing a specific term given a source and a term.
toTermName :: forall leaf fields. (StringConv leaf Text, HasDefaultFields fields) => Source -> SyntaxTerm leaf fields -> Text
toTermName source term = case unwrap term of
  S.Send _ _ -> termNameFromSource term
  S.Ty _ -> termNameFromSource term
  S.TypeDecl id _ -> toTermName' id
  S.TypeAssertion _ _ -> termNameFromSource term
  S.TypeConversion _ _ -> termNameFromSource term
  S.Go expr -> toTermName' expr
  S.Defer expr -> toTermName' expr
  S.AnonymousFunction params _ -> "anonymous" <> paramsToArgNames params
  S.Fixed children -> termNameFromChildren term children
  S.Indexed children -> maybe "branch" sconcat (nonEmpty (intersperse ", " (toTermName' <$> children)))
  Leaf leaf -> toS leaf
  S.Assignment identifier _ -> toTermName' identifier
  S.Function identifier _ _ -> toTermName' identifier
  S.ParameterDecl _ _ -> termNameFromSource term
  S.FunctionCall i _ args -> case unwrap i of
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
  S.MethodCall targetId methodId _ methodParams -> toTermName' targetId <> sep <> toTermName' methodId <> paramsToArgNames methodParams
    where sep = case unwrap targetId of
            S.FunctionCall{} -> "()."
            _ -> "."
  S.SubscriptAccess base element -> case (unwrap base, unwrap element) of
    (S.FunctionCall{}, S.FunctionCall{}) -> toTermName' base <> "()." <> toTermName' element <> "()"
    (S.FunctionCall{}, _) -> toTermName' base <> "()." <> toTermName' element
    (_, S.FunctionCall{}) -> toTermName' base <> "[" <> toTermName' element <> "()" <> "]"
    (S.Indexed _, _) -> case category . extract $ base of
      SliceTy -> termNameFromSource base <> toTermName' element
      _ -> toTermName' base <> "[" <> toTermName' element <> "]"
    (_, _) -> toTermName' base <> "[" <> toTermName' element <> "]"
  S.VarAssignment varId _ -> termNameFromChildren term varId
  S.VarDecl _ -> termNameFromSource term
  -- TODO: We should remove Case from Syntax since I don't think we should ever
  -- evaluate Case as a single toTermName Text - joshvera
  S.Case expr _ -> termNameFromSource expr
  S.Switch exprs _ -> maybe "" toTermName' (fmap snd (unsnoc exprs))
  S.Ternary expr _ -> toTermName' expr
  S.OperatorAssignment id _ -> toTermName' id
  S.Operator _ -> termNameFromSource term
  S.Object ty kvs -> maybe ("{ " <> Text.intercalate ", " (toTermName' <$> kvs) <> " }") termNameFromSource ty
  S.Pair k v -> toKeyName k <> toArgName v
  S.Return children -> Text.intercalate ", " (termNameFromSource <$> children)
  S.Yield children -> Text.intercalate ", " (termNameFromSource <$> children)
  S.ParseError _ -> termNameFromSource term
  S.If expr _ -> termNameFromSource expr
  S.For clauses _ -> termNameFromChildren term clauses
  S.While expr _ -> toTermName' expr
  S.DoWhile _ expr -> toTermName' expr
  S.Throw expr -> termNameFromSource expr
  S.Constructor expr -> toTermName' expr
  S.Try clauses _ _ _ -> termNameFromChildren term clauses
  S.Select clauses -> termNameFromChildren term clauses
  S.Array ty _ -> maybe (termNameFromSource term) termNameFromSource ty
  S.Class identifier _ _ -> toTermName' identifier
  S.Method _ identifier (Just receiver) args _ -> termNameFromSource receiver <> "." <> toTermName' identifier <> paramsToArgNames args
  S.Method _ identifier Nothing args _ -> toTermName' identifier <> paramsToArgNames args
  S.Comment a -> toS a
  S.Commented _ _ -> termNameFromChildren term (toList $ unwrap term)
  S.Module identifier _ -> toTermName' identifier
  S.Namespace identifier _ -> toTermName' identifier
  S.Interface identifier _ _ -> toTermName' identifier
  S.Import identifier [] -> termNameFromSource identifier
  S.Import identifier exprs -> termNameFromChildren term exprs <> " from " <> toTermName' identifier
  S.Export Nothing expr -> "{ " <> Text.intercalate ", " (termNameFromSource <$> expr) <> " }"
  S.Export (Just identifier) [] -> "{ " <> toTermName' identifier <> " }"
  S.Export (Just identifier) expr -> "{ " <> Text.intercalate ", " (termNameFromSource <$> expr) <> " }" <> " from " <> toTermName' identifier
  S.Negate expr -> toTermName' expr
  S.Struct ty _ -> maybe (termNameFromSource term) termNameFromSource ty
  S.Rescue args _ -> Text.intercalate ", " $ toTermName' <$> args
  S.Break expr -> maybe "" toTermName' expr
  S.Continue expr -> maybe "" toTermName' expr
  S.BlockStatement children -> termNameFromChildren term children
  S.DefaultCase children -> termNameFromChildren term children
  S.FieldDecl children -> termNameFromChildren term children
  where toTermName' = toTermName source
        termNameFromChildren term children = termNameFromRange (unionRangesFrom (range term) (range <$> children))
        termNameFromSource term = termNameFromRange (range term)
        termNameFromRange range = toText $ Source.slice range source
        range = byteRange . extract
        paramsToArgNames params = "(" <> Text.intercalate ", " (toArgName <$> params) <> ")"
        toArgName :: SyntaxTerm leaf fields -> Text
        toArgName arg = case identifiable arg of
                          Identifiable arg -> toTermName' arg
                          Unidentifiable _ -> "…"
        toKeyName key = case toTermName' key of
          n | Text.head n == ':' -> n <> " => "
          n -> n <> ": "

parentContexts :: [Either (Category, Text) (Category, Text)] -> Doc
parentContexts contexts = hsep $ either identifiableDoc annotatableDoc <$> contexts
  where
    identifiableDoc (c, t) = case c of
      C.Assignment -> "in an" <+> catName c <+> "to" <+> termName t
      C.Select -> "in a" <+> catName c
      C.Begin -> "in a" <+> catName c
      C.Else -> "in an" <+> catName c
      C.Elsif -> "in the" <+> squotes (termName t) <+> catName c
      C.Method -> "in the" <+> squotes (termName t) <+> catName c
      C.SingletonMethod -> "in the" <+> squotes (termName t) <+> catName c
      C.Ternary -> "in the" <+> squotes (termName t) <+> catName c
      C.Ensure -> "in an" <+> catName c
      C.Rescue -> case t of
        "" -> "in a" <+> catName c
        _ -> "in the" <+> squotes (termName t) <+> catName c
      C.Modifier C.Rescue -> "in the" <+> squotes ("rescue" <+> termName t) <+> "modifier"
      C.If -> "in the" <+> squotes (termName t) <+> catName c
      C.Case -> "in the" <+> squotes (termName t) <+> catName c
      C.Break -> case t of
        "" -> "in a" <+> catName c
        _ -> "in the" <+> squotes (termName t) <+> catName c
      C.Continue -> case t of
        "" -> "in a" <+> catName c
        _ -> "in the" <+> squotes (termName t) <+> catName c
      C.Switch -> case t of
        "" -> "in a" <+> catName c
        _ -> "in the" <+> squotes (termName t) <+> catName c
      C.When -> "in a" <+> catName c
      C.BeginBlock -> "in a" <+> catName c
      C.EndBlock -> "in an" <+> catName c
      C.DefaultCase -> "in a" <+> catName c
      C.TypeDecl -> "in the" <+> squotes (termName t) <+> catName c
      _ -> "in the" <+> termName t <+> catName c
    annotatableDoc (c, t) = "of the" <+> squotes (termName t) <+> catName c
    catName = toDoc . toCategoryName
    termName = toDoc

toDoc :: Text -> Doc
toDoc = string . toS

termToDiffInfo :: (StringConv leaf Text, HasDefaultFields fields) => Source -> SyntaxTerm leaf fields -> DiffInfo
termToDiffInfo blob term = case unwrap term of
  S.Indexed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term) BIndexed
  S.Fixed children -> BranchInfo (termToDiffInfo' <$> children) (category $ extract term) BFixed
  S.AnonymousFunction _ _ -> LeafInfo C.AnonymousFunction (toTermName' term) (getField $ extract term)
  S.Comment _ -> HideInfo
  S.Commented cs leaf -> BranchInfo (termToDiffInfo' <$> cs <> maybeToList leaf) (category $ extract term) BCommented
  S.ParseError _ -> ErrorInfo (getField $ extract term) (toTermName' term)
  _ -> toLeafInfo term
  where toTermName' = toTermName blob
        termToDiffInfo' = termToDiffInfo blob
        toLeafInfo term = LeafInfo (category $ extract term) (toTermName' term) (getField $ extract term)

-- | Append a parentAnnotation to the current DiffSummary instance.
-- | For a DiffSummary without a parentAnnotation, we append a parentAnnotation with the first identifiable term.
-- | For a DiffSummary with a parentAnnotation, we append the next annotatable term to the extant parentAnnotation.
-- | If a DiffSummary already has a parentAnnotation, and a (grand) parentAnnotation, then we return the summary without modification.
appendSummary :: (StringConv leaf Text, HasDefaultFields fields) => Source -> SyntaxTerm leaf fields -> DiffSummary DiffInfo -> DiffSummary DiffInfo
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
  toCategoryName category = case category of
    C.Ty -> "type"
    ArrayLiteral -> "array"
    BooleanOperator -> "boolean operator"
    MathOperator -> "math operator"
    BitwiseOperator -> "bitwise operator"
    RelationalOperator -> "relational operator"
    Boolean -> "boolean"
    DictionaryLiteral -> "dictionary"
    C.Comment -> "comment"
    C.ParseError -> "error"
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
    FloatLiteral -> "float"
    Other s -> s
    C.Pair -> "pair"
    C.Params -> "params"
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
    C.Module -> "module"
    C.Namespace -> "namespace"
    C.Interface -> "interface"
    C.Import -> "import statement"
    C.Export -> "export statement"
    C.AnonymousFunction -> "anonymous function"
    C.Interpolation -> "interpolation"
    C.Subshell -> "subshell command"
    C.OperatorAssignment -> "operator assignment"
    C.Yield -> "yield statement"
    C.Until -> "until statement"
    C.Unless -> "unless statement"
    C.Begin -> "begin statement"
    C.Else -> "else block"
    C.Elsif -> "elsif block"
    C.Ensure -> "ensure block"
    C.Rescue -> "rescue block"
    C.RescueModifier -> "rescue modifier"
    C.When -> "when comparison"
    C.RescuedException -> "last exception"
    C.RescueArgs -> "arguments"
    C.Negate -> "negate"
    C.Select -> "select statement"
    C.Go -> "go statement"
    C.Slice -> "slice literal"
    C.Defer -> "defer statement"
    C.TypeAssertion -> "type assertion statement"
    C.TypeConversion -> "type conversion expression"
    C.ArgumentPair -> "argument"
    C.KeywordParameter -> "parameter"
    C.OptionalParameter -> "parameter"
    C.SplatParameter -> "parameter"
    C.HashSplatParameter -> "parameter"
    C.BlockParameter -> "parameter"
    C.ArrayTy -> "array type"
    C.DictionaryTy -> "dictionary type"
    C.StructTy -> "struct type"
    C.Struct -> "struct"
    C.Break -> "break statement"
    C.Continue -> "continue statement"
    C.Binary -> "binary statement"
    C.Unary -> "unary statement"
    C.Constant -> "constant"
    C.Superclass -> "superclass"
    C.SingletonClass -> "singleton class"
    C.SingletonMethod -> "method"
    C.RangeExpression -> "range"
    C.ScopeOperator -> "scope operator"
    C.BeginBlock -> "BEGIN block"
    C.EndBlock -> "END block"
    C.ParameterDecl -> "parameter declaration"
    C.DefaultCase -> "default statement"
    C.TypeDecl -> "type declaration"
    C.PointerTy -> "pointer type"
    C.FieldDecl -> "field declaration"
    C.SliceTy -> "slice type"
    C.Element -> "element"
    C.Literal -> "literal"
    C.ChannelTy -> "channel type"
    C.Send -> "send statement"
    C.IndexExpression -> "index expression"
    C.FunctionTy -> "function type"
    C.IncrementStatement -> "increment statement"
    C.DecrementStatement -> "decrement statement"
    C.QualifiedIdentifier -> "qualified identifier"
    C.FieldDeclarations -> "field declarations"
    C.RuneLiteral -> "rune literal"
    C.Modifier C.Rescue -> "rescue modifier"
    C.Modifier c -> toCategoryName c

instance HasField fields Category => HasCategory (SyntaxTerm leaf fields) where
  toCategoryName = toCategoryName . category . extract

instance Listable Branch where
  tiers = cons0 BIndexed \/ cons0 BFixed \/ cons0 BCommented \/ cons0 BIf

instance Listable1 DiffSummary where
  liftTiers termTiers = liftCons2 (liftTiers termTiers) (liftTiers (eitherTiers (liftTiers (mapT unListableText tiers)))) DiffSummary
    where eitherTiers tiers = liftTiers2 tiers tiers

instance Listable a => Listable (DiffSummary a) where
  tiers = tiers1

instance P.Pretty DiffInfo where
  pretty LeafInfo{..} = squotes (string $ toSL termName) <+> string (toSL (toCategoryName leafCategory))
  pretty BranchInfo{..} = mconcat $ punctuate (string "," P.<> space) (pretty <$> branches)
  pretty ErrorInfo{..} = squotes (string $ toSL termName) <+> "at" <+> (string . toSL $ displayStartEndPos errorSpan)
  pretty HideInfo = ""
