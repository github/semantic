{-# OPTIONS_GHC -Wno-deprecations #-}
-- Disabling deprecation warnings due to pattern match against RescueModifier.
module Renderer.Split (split) where

import Alignment
import Category as C
import Data.Bifunctor.Join
import Data.Foldable
import Data.Functor.Both
import Data.Record
import qualified Data.Text.Lazy as TL
import Data.These
import Info
import Prologue hiding (div, head, fst, snd, link, (<>))
import qualified Prologue
import Renderer
import Source
import SplitDiff
import Syntax
import Term
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as Blaze

-- | Add the first category from a Foldable of categories as a class name as a
-- | class name on the markup, prefixed by `category-`.
classifyMarkup :: Category -> Markup -> Markup
classifyMarkup category element = (element !) . A.class_ . textValue $ styleName category

-- | Return the appropriate style name for the given category.
styleName :: Category -> Text
styleName category = "category-" <> case category of
  Program -> "program"
  C.ParseError -> "error"
  BooleanOperator -> "boolean_operator"
  MathOperator -> "math_operator"
  BitwiseOperator -> "bitwise_operator"
  RelationalOperator -> "relational_operator"
  Boolean -> "boolean"
  DictionaryLiteral -> "dictionary"
  C.Pair -> "pair"
  StringLiteral -> "string"
  SymbolLiteral -> "symbol"
  IntegerLiteral -> "integer"
  NumberLiteral -> "number"
  FloatLiteral -> "float"
  C.Comment -> "comment"
  C.FunctionCall -> "function_call"
  C.Function -> "function"
  C.MethodCall -> "method_call"
  C.Args -> "arguments"
  C.Assignment -> "assignment"
  C.MemberAccess -> "member_access"
  C.VarDecl -> "var_declaration"
  C.VarAssignment -> "var_assignment"
  C.Switch -> "switch"
  C.Case -> "case"
  TemplateString -> "template_string"
  Regex -> "regex"
  Identifier -> "identifier"
  C.Params -> "parameters"
  ExpressionStatements -> "expression_statements"
  C.MathAssignment -> "math_assignment"
  C.SubscriptAccess -> "subscript_access"
  C.Ternary -> "ternary"
  C.Operator -> "operator"
  C.Object -> "object"
  C.For -> "for"
  C.While -> "while"
  C.DoWhile -> "do_while"
  C.Return -> "return_statement"
  C.Throw -> "throw_statement"
  C.Constructor -> "constructor"
  C.Try -> "try_statement"
  C.Catch -> "catch_statement"
  C.Finally -> "finally_statement"
  ArrayLiteral -> "array"
  C.Class -> "class_statement"
  C.Method -> "method"
  C.If -> "if_statement"
  C.Empty -> "empty_statement"
  C.CommaOperator -> "comma_operator"
  Other string -> string
  C.Module -> "module_statement"
  C.Import -> "import_statement"
  C.Export -> "export_statement"
  C.AnonymousFunction -> "anonymous_function"
  C.Interpolation -> "interpolation"
  C.Subshell -> "subshell"
  C.OperatorAssignment -> "operator_assignment"
  C.Yield -> "yield_expression"
  C.Until -> "until"
  C.Unless -> "unless_statement"
  C.Begin -> "begin_statement"
  C.Else -> "else_block"
  C.Elsif -> "elsif_block"
  C.Ensure -> "ensure_block"
  C.Rescue -> "rescue_block"
  C.RescueModifier -> "rescue_modifier"
  C.When -> "when_block"
  C.RescuedException -> "last_exception"
  C.RescueArgs -> "rescue_args"
  C.Negate -> "negate"
  C.Select -> "select_statement"
  C.Go -> "go_statement"
  C.Defer -> "defer_statement"
  C.Slice -> "slice_expression"
  C.TypeAssertion -> "type_assertion"
  C.TypeConversion -> "type_conversion"
  C.ArgumentPair -> "argument_pair"
  C.KeywordParameter -> "keyword_param"
  C.OptionalParameter -> "optional_param"
  C.SplatParameter -> "splat_param"
  C.HashSplatParameter -> "hash_splat_param"
  C.BlockParameter -> "block_param"
  C.ArrayTy -> "array_type"
  C.DictionaryTy -> "dictionary_type"
  C.StructTy -> "struct_type"
  C.Struct -> "struct"
  C.Break -> "break_statement"
  C.Continue -> "continue_statement"
  C.Binary -> "binary"
  C.Unary -> "unary"
  C.Constant -> "constant"
  C.Superclass -> "superclass"
  C.SingletonClass -> "singleton_class"
  C.SingletonMethod -> "singleton_method"
  C.RangeExpression -> "range"
  C.ScopeOperator -> "scope_operator"
  C.BeginBlock -> "begin_block"
  C.EndBlock -> "end_block"
  C.ParameterDecl -> "parameter_declaration"
  C.DefaultCase -> "default_statement"
  C.TypeDecl -> "type_declaration"
  C.PointerTy -> "pointer_type"
  C.FieldDecl -> "field_declaration"
  C.SliceTy -> "slice_type"
  C.Element -> "element"
  C.Literal -> "literal"
  C.ChannelTy -> "channel_type"
  C.FunctionTy -> "function_type"
  C.Send -> "send_statement"
  C.IncrementStatement -> "increment_statement"
  C.DecrementStatement -> "decrement_statement"
  C.QualifiedIdentifier -> "qualified_identifier"
  C.IndexExpression -> "index_expression"
  C.FieldDeclarations -> "field_declarations"
  C.RuneLiteral -> "rune_literal"
  C.Modifier c -> styleName c <> "_modifier"
  C.Ty -> "type"

-- | Pick the class name for a split patch.
splitPatchToClassName :: SplitPatch a -> AttributeValue
splitPatchToClassName patch = stringValue $ "patch " <> case patch of
  SplitInsert _ -> "insert"
  SplitDelete _ -> "delete"
  SplitReplace _ -> "replace"

-- | Render a diff as an HTML split diff.
split :: (HasField fields Category, HasField fields Range) => Renderer (Record fields)
split blobs diff = SplitOutput . TL.toStrict . renderHtml
  . docTypeHtml
    . ((head $ link ! A.rel "stylesheet" ! A.href "style.css") <>)
    . body
      . (table ! A.class_ (stringValue "diff")) .
        ((colgroup $ (col ! A.width (stringValue . show $ columnWidth)) <> col <> (col ! A.width (stringValue . show $ columnWidth)) <> col) <>)
        . mconcat $ numberedLinesToMarkup <$> numbered
  where
    sources = Source.source <$> blobs
    numbered = numberedRows (alignDiff sources diff)
    maxNumber = case numbered of
      [] -> 0
      (row : _) -> mergeThese max . runJoin $ Prologue.fst <$> row

    -- | The number of digits in a number (e.g. 342 has 3 digits).
    digits :: Int -> Int
    digits n = let base = 10 :: Int in
      ceiling (logBase (fromIntegral base) (fromIntegral n) :: Double)

    columnWidth = max (20 + digits maxNumber * 8) 40

    -- | Render a line with numbers as an HTML row.
    numberedLinesToMarkup numberedLines = tr $ runBothWith (<>) (renderLine <$> Join (fromThese Nothing Nothing (runJoin (Just <$> numberedLines))) <*> sources) <> string "\n"

    renderLine (Just (number, line)) source = toMarkup $ Cell (hasChanges line) number (Renderable source line)
    renderLine _ _
      =  (td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell"))
      <> (td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell"))
      <> string "\n"

-- | A cell in a table, characterized by whether it contains changes & its line number.
data Cell a = Cell !Bool !Int !a

-- | Something that can be rendered as markup with reference to some source.
data Renderable a = Renderable !Source !a

contentElements :: (Foldable t, ToMarkup f) => Source -> Range -> t (f, Range) -> [Markup]
contentElements source range children = let (elements, next) = foldr' (markupForContextAndChild source) ([], end range) children in
  text (toText (slice (Range (start range) (max next (start range))) source)) : elements

markupForContextAndChild :: ToMarkup f => Source -> (f, Range) -> ([Markup], Int) -> ([Markup], Int)
markupForContextAndChild source (child, range) (rows, next) = (toMarkup child : text (toText (slice (Range (end range) next) source)) : rows, start range)

wrapIn :: (Markup -> Markup) -> Markup -> Markup
wrapIn _ l@Blaze.Leaf{} = l
wrapIn _ l@Blaze.CustomLeaf{} = l
wrapIn _ l@Blaze.Content{} = l
wrapIn _ l@Blaze.Comment{} = l
wrapIn f p = f p


-- Instances

instance (ToMarkup f, HasField fields Category, HasField fields Range) => ToMarkup (Renderable (SyntaxTermF leaf fields (f, Range))) where
  toMarkup (Renderable source (info :< syntax)) = classifyMarkup (category info) $ case syntax of
    Leaf _ -> span . text . toText $ slice (byteRange info) source
    _ -> ul . mconcat $ wrapIn li <$> contentElements source (byteRange info) (toList syntax)

instance (HasField fields Category, HasField fields Range) => ToMarkup (Renderable (SyntaxTerm leaf fields)) where
  toMarkup (Renderable source term) = Prologue.fst $ cata (\ t -> (toMarkup $ Renderable source t, byteRange (headF t))) term

instance (HasField fields Category, HasField fields Range) => ToMarkup (Renderable (SplitSyntaxDiff leaf fields)) where
  toMarkup (Renderable source diff) = Prologue.fst . iter (\ t -> (toMarkup $ Renderable source t, byteRange (headF t))) $ toMarkupAndRange <$> diff
    where toMarkupAndRange patch = let term@(info :< _) = runCofree $ getSplitTerm patch in
            ((div ! patchAttribute patch) . toMarkup $ Renderable source (cofree term), byteRange info)
          patchAttribute patch = A.class_ (splitPatchToClassName patch)

instance ToMarkup a => ToMarkup (Cell a) where
  toMarkup (Cell hasChanges num line)
    =  (td (string (show num)) ! A.class_ (stringValue $ if hasChanges then "blob-num blob-num-replacement" else "blob-num"))
    <> (td (toMarkup line) ! A.class_ (stringValue $ if hasChanges then "blob-code blob-code-replacement" else "blob-code"))
    <> string "\n"

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
