{-# LANGUAGE FlexibleInstances #-}
module Renderer.Split where

import Alignment
import Prelude hiding (div, head, span)
import Category
import Diff
import Line
import Row
import Renderer
import Term
import SplitDiff
import Syntax
import Control.Comonad.Cofree
import Range
import Control.Monad.Free
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text
import Data.Functor.Both
import Data.Foldable
import Data.Monoid
import Source hiding ((++))

type ClassName = T.Text

-- | Add the first category from a Foldable of categories as a class name as a
-- | class name on the markup, prefixed by `category-`.
classifyMarkup :: Foldable f => f Category -> Markup -> Markup
classifyMarkup categories element = maybe element ((element !) . A.class_ . stringValue . styleName) $ maybeFirst categories

-- | Return the appropriate style name for the given category.
styleName :: Category -> String
styleName category = "category-" ++ case category of
  BinaryOperator -> "binary-operator"
  DictionaryLiteral -> "dictionary"
  Pair -> "pair"
  FunctionCall -> "function_call"
  StringLiteral -> "string"
  SymbolLiteral -> "symbol"
  IntegerLiteral -> "integer"
  Other string -> string

-- | Pick the class name for a split patch.
splitPatchToClassName :: SplitPatch a -> AttributeValue
splitPatchToClassName patch = stringValue $ "patch " ++ case patch of
  SplitInsert _ -> "insert"
  SplitDelete _ -> "delete"
  SplitReplace _ -> "replace"

-- | Render a diff as an HTML split diff.
split :: Renderer leaf TL.Text
split diff blobs = renderHtml
  . docTypeHtml
    . ((head $ link ! A.rel "stylesheet" ! A.href "style.css") <>)
    . body
      . (table ! A.class_ (stringValue "diff")) $
        ((colgroup $ (col ! A.width (stringValue . show $ columnWidth)) <> col <> (col ! A.width (stringValue . show $ columnWidth)) <> col) <>)
        . mconcat $ numberedLinesToMarkup <$> reverse numbered
  where
    sources = Source.source <$> blobs
    rows = fst (splitDiffByLines diff (pure 0) sources)
    numbered = foldl' numberRows [] rows
    maxNumber = case numbered of
      [] -> 0
      (row : _) -> uncurry max . runBoth $ fst <$> row

    -- | The number of digits in a number (e.g. 342 has 3 digits).
    digits :: Int -> Int
    digits n = let base = 10 :: Int in
      ceiling (logBase (fromIntegral base) (fromIntegral n) :: Double)

    columnWidth = max (20 + digits maxNumber * 8) 40

    -- | Render a line with numbers as an HTML row.
    numberedLinesToMarkup :: Both (Int, Line (SplitDiff a Info)) -> Markup
    numberedLinesToMarkup numberedLines = tr $ uncurry (<>) (runBoth (renderLine <$> numberedLines <*> sources)) <> string "\n"

    renderLine :: (Int, Line (SplitDiff leaf Info)) -> Source Char -> Markup
    renderLine (number, line) source = toMarkup $ Renderable (or $ hasChanges <$> line, number, Renderable . (,) source <$> line)

    hasChanges diff = or $ const True <$> diff

    -- | Add a row to list of tuples of ints and lines, where the ints denote
    -- | how many non-empty lines exist on that side up to that point.
    numberRows :: [Both (Int, Line a)] -> Row a -> [Both (Int, Line a)]
    numberRows rows row = ((,) <$> ((+) <$> count rows <*> (valueOf <$> unRow row)) <*> unRow row) : rows
      where count = maybe (pure 0) (fmap fst) . maybeFirst
            valueOf EmptyLine = 0
            valueOf _ = 1

-- | Something that can be rendered as markup.
newtype Renderable a = Renderable a

instance ToMarkup f => ToMarkup (Renderable (Source Char, Info, Syntax a (f, Range))) where
  toMarkup (Renderable (source, Info range categories, syntax)) = classifyMarkup categories $ case syntax of
    Leaf _ -> span . string . toString $ slice range source
    Indexed children -> ul . mconcat $ wrapIn li <$> contentElements children
    Fixed children -> ul . mconcat $ wrapIn li <$> contentElements children
    Keyed children -> dl . mconcat $ wrapIn dd <$> contentElements children
    where markupForSeparatorAndChild :: ToMarkup f => ([Markup], Int) -> (f, Range) -> ([Markup], Int)
          markupForSeparatorAndChild (rows, previous) (child, range) = (rows ++ [ string  (toString $ slice (Range previous $ start range) source), toMarkup child ], end range)

          wrapIn _ l@Blaze.Leaf{} = l
          wrapIn _ l@Blaze.CustomLeaf{} = l
          wrapIn _ l@Blaze.Content{} = l
          wrapIn _ l@Blaze.Comment{} = l
          wrapIn f p = f p

          contentElements children = let (elements, previous) = foldl' markupForSeparatorAndChild ([], start range) children in
            elements ++ [ string . toString $ slice (Range previous $ end range) source ]

instance ToMarkup (Renderable (Source Char, Term a Info)) where
  toMarkup (Renderable (source, term)) = fst $ cata (\ info@(Info range _) syntax -> (toMarkup $ Renderable (source, info, syntax), range)) term

instance ToMarkup (Renderable (Source Char, SplitDiff a Info)) where
  toMarkup (Renderable (source, diff)) = fst $ iter (\ (Annotated info@(Info range _) syntax) -> (toMarkup $ Renderable (source, info, syntax), range)) $ toMarkupAndRange <$> diff
    where toMarkupAndRange :: SplitPatch (Term a Info) -> (Markup, Range)
          toMarkupAndRange patch = let term@(Info range _ :< _) = getSplitTerm patch in
            ((div ! A.class_ (splitPatchToClassName patch) ! A.data_ (stringValue . show $ termSize term)) . toMarkup $ Renderable (source, term), range)


instance ToMarkup a => ToMarkup (Renderable (Bool, Int, Line a)) where
  toMarkup (Renderable (_, _, EmptyLine)) = td mempty ! A.class_ (stringValue "blob-num blob-num-empty empty-cell") <> td mempty ! A.class_ (stringValue "blob-code blob-code-empty empty-cell") <> string "\n"
  toMarkup (Renderable (hasChanges, num, line))
    = td (string $ show num) ! A.class_ (stringValue $ if hasChanges then "blob-num blob-num-replacement" else "blob-num")
    <> td (mconcat $ toMarkup <$> unLine line) ! A.class_ (stringValue $ if hasChanges then "blob-code blob-code-replacement" else "blob-code") <> string "\n"
