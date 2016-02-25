{-# LANGUAGE NoOverloadedStrings #-}
module Renderer.JSON (
  json
) where

import Diff
import Line
import Range
import Row
import Renderer
import Renderer.Split
import Source hiding ((++), toList)
import Syntax
import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Foldable

-- | JSON representing an aligned diff.
newtype JSON a = JSON { rows :: [Row (SplitDiff a Info)] }

-- | Render a diff to a string representing its JSON.
json :: Renderer a String
json diff (a, b) = show . JSON . fst $ splitDiffByLines diff (0, 0) (source a, source b)

instance Show (JSON a) where
  show (JSON rows) = "{'rows':[" ++ mconcat (showRow <$> rows) ++ "]}"
    where showRow (Row left right) = "{'left':" ++ showLine left ++ ",'right':" ++ showLine right ++ "}"
          showLine EmptyLine = "null"
          showLine (Line diffs) = mconcat (showDiff <$> toList diffs)
          showDiff (Pure patch) = showPatch patch
          showDiff (Free (Annotated info syntax)) = showInfoSyntax info syntax
          showPatch (SplitInsert term) = "{'insert':" ++ showTerm term ++ "}"
          showPatch (SplitDelete term) = "{'delete':" ++ showTerm term ++ "}"
          showPatch (SplitReplace term) = "{'replace':" ++ showTerm term ++ "}"
          showTerm (info :< syntax) = showInfoSyntax info syntax
          showInfoSyntax (Info range categories) syntax = "{'range':" ++ showRange range ++ ",'categories':" ++ showCategories categories ++ "," ++ showSyntax syntax ++ "}"
          showRange (Range start end) = "{'start':" ++ show start ++ ",'end':" ++ show end ++ "}"
          showCategories categories = "{" ++ mconcat (show <$> toList categories) ++ "}"
          showSyntax (Leaf _) = "type:'leaf'"
          showSyntax (Indexed i) = "{}"
          showSyntax (Fixed f) = "{}"
          showSyntax (Keyed k) = "{}"
