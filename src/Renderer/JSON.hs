{-# LANGUAGE NoOverloadedStrings #-}
module Renderer.JSON (
  json
) where

import Alignment
import Diff
import Line
import qualified Data.OrderedMap as Map
import Range
import Row
import Renderer
import Source hiding ((++), toList)
import SplitDiff
import Syntax
import Term
import Control.Arrow
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
          showLine (Line diffs) = "[" ++ intercalate "," (showDiff <$> toList diffs) ++ "]"
          showDiff diff = iter (uncurry showInfoSyntax . (getAnnotation &&& getSyntax)) (showPatch <$> diff)
          showPatch (SplitInsert term) = "{'insert':" ++ cata showInfoSyntax term ++ "}"
          showPatch (SplitDelete term) = "{'delete':" ++ cata showInfoSyntax term ++ "}"
          showPatch (SplitReplace term) = "{'replace':" ++ cata showInfoSyntax term ++ "}"
          showInfoSyntax (Info range categories) syntax = "{'range':" ++ showRange range ++ ",'categories':" ++ showCategories categories ++ "," ++ showSyntax syntax ++ "}"
          showRange (Range start end) = "{'start':" ++ show start ++ ",'end':" ++ show end ++ "}"
          showCategories categories = "{" ++ intercalate "," (show <$> toList categories) ++ "}"
          showSyntax (Leaf _) = "'type':'leaf'"
          showSyntax (Indexed children) = "'type':'indexed','children':[" ++ intercalate "," children ++ "]"
          showSyntax (Fixed children) = "'type':'fixed','children':[" ++ intercalate "," children ++ "]"
          showSyntax (Keyed children) = "'type':'keyed','children':{" ++ intercalate "," (uncurry showKeyValue <$> Map.toList children) ++ "}"
          showKeyValue key value = "'" ++ show key ++ "': " ++ value
