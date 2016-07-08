{-# LANGUAGE OverloadedStrings #-}
module Renderer.Split where

import Alignment
import Category as C
import Data.Bifunctor.Join
import Data.Foldable
import Data.Functor.Both
import Data.Functor.Foldable (cata)
import qualified Data.Text.Lazy as TL
import Data.These
import Info
import Prologue hiding (div, head, fst, snd, link)
import qualified Prologue
import Range
import Renderer
import Source hiding ((++))
import SplitDiff
import Syntax
import Term
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as Blaze

-- | Return the first item in the Foldable, or Nothing if it's empty.
maybeFirst :: Foldable f => f a -> Maybe a
maybeFirst = foldr (const . Just) Nothing

-- | Add the first category from a Foldable of categories as a class name as a
-- | class name on the markup, prefixed by `category-`.
classifyMarkup :: Category -> Markup -> Markup
classifyMarkup category element = (element !) . A.class_ . textValue $ styleName category

-- | Return the appropriate style name for the given category.
styleName :: Category -> Text
styleName category = "category-" <> case category of
  Program -> "program"
  Error -> "error"
  BinaryOperator -> "binary-operator"
  Boolean -> "boolean"
  DictionaryLiteral -> "dictionary"
  C.Pair -> "pair"
  StringLiteral -> "string"
  SymbolLiteral -> "symbol"
  IntegerLiteral -> "integer"
  ArrayLiteral -> "array"
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
  Params -> "parameters"
  ExpressionStatements -> "expression_statements"
  C.MathAssignment -> "math_assignment"
  C.SubscriptAccess -> "subscript_access"
  C.Ternary -> "ternary"
  C.Operator -> "operator"
  C.Object -> "object"
  Other string -> string

-- | Pick the class name for a split patch.
splitPatchToClassName :: SplitPatch a -> AttributeValue
splitPatchToClassName patch = stringValue $ "patch " ++ case patch of
  SplitInsert _ -> "insert"
  SplitDelete _ -> "delete"
  SplitReplace _ -> "replace"

-- | Render a diff as an HTML split diff.
split :: Renderer
split diff blobs = TL.toStrict . renderHtml
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
    numberedLinesToMarkup :: Join These (Int, SplitDiff a Info) -> Markup
    numberedLinesToMarkup numberedLines = tr $ runBothWith (<>) (renderLine <$> Join (fromThese Nothing Nothing (runJoin (Just <$> numberedLines))) <*> sources) <> string "\n"

    renderLine :: Maybe (Int, SplitDiff leaf Info) -> Source Char -> Markup
    renderLine (Just (number, line)) source = toMarkup $ Renderable (hasChanges line, number, Renderable (source, line))
    renderLine _ _ =
      td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell")
      <> td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell")
      <> string "\n"

-- | Something that can be rendered as markup.
newtype Renderable a = Renderable a

instance ToMarkup f => ToMarkup (Renderable (Source Char, Info, Syntax a (f, Range))) where
  toMarkup (Renderable (source, info, syntax)) = (! A.data_ (textValue (show . unSize $ size info))) . classifyMarkup (category info) $ case syntax of
    Leaf _ -> span . text . toText $ slice (characterRange info) source
    Comment _ -> span . text . toText $ slice (characterRange info) source
    Commented cs child -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) (cs <> maybeToList child)
    Indexed children -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) children
    Fixed children -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) children
    Keyed children -> dl . mconcat $ wrapIn dd <$> contentElements source (characterRange info) children
    Syntax.FunctionCall identifier children -> dl . mconcat $ (wrapIn dt <$> (contentElements source (characterRange info) [identifier])) <> (wrapIn dd <$> contentElements source (characterRange info) children)
    Syntax.Function identifier params expressions -> ul . mconcat $ wrapIn li <$>
      contentElements source (characterRange info) (catMaybes [identifier, params, Just expressions])
    Syntax.MethodCall targetId methodId methodParams -> ul . mconcat $ wrapIn li <$>
      contentElements source (characterRange info) [targetId, methodId, methodParams]
    Syntax.Args children -> ul . mconcat $ wrapIn li <$>
      contentElements source (characterRange info) children
    Syntax.MemberAccess memberId property -> ul . mconcat $ wrapIn li <$>
      contentElements source (characterRange info) [memberId, property]
    Syntax.Assignment  memberId value -> ul . mconcat $ wrapIn li <$>
      contentElements source (characterRange info) [memberId, value]
    Syntax.VarDecl decl -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) [decl]
    Syntax.VarAssignment varId value ->
      dl . mconcat $ (wrapIn dt <$> (contentElements source (characterRange info) [varId])) <> (wrapIn dd <$> contentElements source (characterRange info) [value])
    Syntax.Switch expr cases -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) (expr : cases)
    Syntax.Case expr body -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) [expr, body]
    Syntax.Ternary expr cases -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) (expr : cases)
    Syntax.MathAssignment id value ->
      dl . mconcat $ (wrapIn dt <$> (contentElements source (characterRange info) [id])) <> (wrapIn dd <$> contentElements source (characterRange info) [value])
    Syntax.SubscriptAccess target property ->
      dl . mconcat $ (wrapIn dt <$> (contentElements source (characterRange info) [target])) <> (wrapIn dd <$> contentElements source (characterRange info) [property])
    Syntax.Operator syntaxes -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) syntaxes
    Syntax.Object children -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) children
    Syntax.Pair a b -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) [a, b]

contentElements :: (Foldable t, ToMarkup f) => Source Char -> Range -> t (f, Range) -> [Markup]
contentElements source range children = let (elements, next) = foldr' (markupForContextAndChild source) ([], end range) children in
  text (toText (slice (Range (start range) (max next (start range))) source)) : elements

markupForContextAndChild :: ToMarkup f => Source Char -> (f, Range) -> ([Markup], Int) -> ([Markup], Int)
markupForContextAndChild source (child, range) (rows, next) = (toMarkup child : text (toText (slice (Range (end range) next) source)) : rows, start range)

wrapIn :: (Markup -> Markup) -> Markup -> Markup
wrapIn _ l@Blaze.Leaf{} = l
wrapIn _ l@Blaze.CustomLeaf{} = l
wrapIn _ l@Blaze.Content{} = l
wrapIn _ l@Blaze.Comment{} = l
wrapIn f p = f p

instance ToMarkup (Renderable (Source Char, Term a Info)) where
  toMarkup (Renderable (source, term)) = Prologue.fst $ cata (\ (info :< syntax) -> (toMarkup $ Renderable (source, info, syntax), characterRange info)) term

instance ToMarkup (Renderable (Source Char, SplitDiff a Info)) where
  toMarkup (Renderable (source, diff)) = Prologue.fst . iter (\ (info :< syntax) -> (toMarkup $ Renderable (source, info, syntax), characterRange info)) $ toMarkupAndRange <$> diff
    where toMarkupAndRange :: SplitPatch (Term a Info) -> (Markup, Range)
          toMarkupAndRange patch = let term@(info :< _) = runCofree $ getSplitTerm patch in
            ((div ! A.class_ (splitPatchToClassName patch) ! A.data_ (stringValue (show (unSize (size info))))) . toMarkup $ Renderable (source, cofree term), characterRange info)

instance ToMarkup a => ToMarkup (Renderable (Bool, Int, a)) where
  toMarkup (Renderable (hasChanges, num, line)) =
    td (string $ show num) ! A.class_ (stringValue $ if hasChanges then "blob-num blob-num-replacement" else "blob-num")
    <> td (toMarkup line) ! A.class_ (stringValue $ if hasChanges then "blob-code blob-code-replacement" else "blob-code")
    <> string "\n"
