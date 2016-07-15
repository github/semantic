module Renderer.Split (split) where

import Data.String
import Alignment
import Category
import Data.Bifunctor.Join
import Data.Foldable
import Data.Functor.Both
import Data.Functor.Foldable (cata)
import Data.Record
import qualified Data.Text.Lazy as TL
import Data.These
import Info
import Prologue hiding (div, head, fst, snd, link)
import qualified Prologue
import Range
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

-- | Return the first item in the Foldable, or Nothing if it's empty.
maybeFirst :: Foldable f => f a -> Maybe a
maybeFirst = foldr (const . Just) Nothing

-- | Add the first category from a Foldable of categories as a class name as a
-- | class name on the markup, prefixed by `category-`.
classifyMarkup :: Category -> Markup -> Markup
classifyMarkup category element = (element !) . A.class_ . stringValue $ styleName category

-- | Return the appropriate style name for the given category.
styleName :: Category -> String
styleName category = "category-" <> case category of
  Program -> "program"
  Error -> "error"
  BinaryOperator -> "binary-operator"
  DictionaryLiteral -> "dictionary"
  Pair -> "pair"
  FunctionCall -> "function_call"
  StringLiteral -> "string"
  SymbolLiteral -> "symbol"
  IntegerLiteral -> "integer"
  ArrayLiteral -> "array"
  Other string -> string

-- | Pick the class name for a split patch.
splitPatchToClassName :: SplitPatch a -> AttributeValue
splitPatchToClassName patch = stringValue $ "patch " <> case patch of
  SplitInsert _ -> "insert"
  SplitDelete _ -> "delete"
  SplitReplace _ -> "replace"

-- | Render a diff as an HTML split diff.
split :: (HasField fields Category, HasField fields Cost, HasField fields Range) => Renderer (Record fields)
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
    numberedLinesToMarkup numberedLines = tr $ runBothWith (<>) (renderLine <$> Join (fromThese Nothing Nothing (runJoin (Just <$> numberedLines))) <*> sources) <> string "\n"

    renderLine (Just (number, line)) source = toMarkup $ Cell (hasChanges line) number (Renderable source line)
    renderLine _ _ =
      td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell")
      <> td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell")
      <> string "\n"

-- | Something that can be rendered as markup.
data Cell a = Cell { containsChanges :: !Bool, lineNumber :: !Int, getCell :: !a }
data Renderable a = Renderable { source :: !(Source Char), contents :: !a }

contentElements :: (Foldable t, ToMarkup f) => Source Char -> Range -> t (f, Range) -> [Markup]
contentElements source range children = let (elements, next) = foldr' (markupForContextAndChild source) ([], end range) children in
  string (toString (slice (Range (start range) (max next (start range))) source)) : elements

markupForContextAndChild :: ToMarkup f => Source Char -> (f, Range) -> ([Markup], Int) -> ([Markup], Int)
markupForContextAndChild source (child, range) (rows, next) = (toMarkup child : string (toString (slice (Range (end range) next) source)) : rows, start range)

wrapIn :: (Markup -> Markup) -> Markup -> Markup
wrapIn _ l@Blaze.Leaf{} = l
wrapIn _ l@Blaze.CustomLeaf{} = l
wrapIn _ l@Blaze.Content{} = l
wrapIn _ l@Blaze.Comment{} = l
wrapIn f p = f p


-- Instances

instance (ToMarkup f, HasField fields Category, HasField fields Cost, HasField fields Range) => ToMarkup (Renderable (CofreeF (Syntax leaf) (Record fields) (f, Range))) where
  toMarkup (Renderable source (info :< syntax)) = classifyMarkup (category info) $ case syntax of
    Leaf _ -> span . string . toString $ slice (characterRange info) source
    _ -> ul . mconcat $ wrapIn li <$> contentElements source (characterRange info) (toList syntax)

instance (HasField fields Category, HasField fields Cost, HasField fields Range) => ToMarkup (Renderable (Term leaf (Record fields))) where
  toMarkup (Renderable source term) = Prologue.fst $ cata (\ t -> (toMarkup $ Renderable source t, characterRange (headF t))) term

instance (HasField fields Category, HasField fields Cost, HasField fields Range) => ToMarkup (Renderable (SplitDiff leaf (Record fields))) where
  toMarkup (Renderable source diff) = Prologue.fst . iter (\ t -> (toMarkup $ Renderable source t, characterRange (headF t))) $ toMarkupAndRange <$> diff
    where toMarkupAndRange patch = let term@(info :< _) = runCofree $ getSplitTerm patch in
            ((div ! patchAttribute patch `withCostAttribute` cost info) . toMarkup $ Renderable source (cofree term), characterRange info)
          patchAttribute patch = A.class_ (splitPatchToClassName patch)
          withCostAttribute a (Cost c) | c > 0 = a ! A.data_ (stringValue (show c))
                                       | otherwise = identity

instance ToMarkup a => ToMarkup (Cell a) where
  toMarkup (Cell hasChanges num line) =
    td (string $ show num) ! A.class_ (stringValue $ if hasChanges then "blob-num blob-num-replacement" else "blob-num")
    <> td (toMarkup line) ! A.class_ (stringValue $ if hasChanges then "blob-code blob-code-replacement" else "blob-code")
    <> string "\n"
