{-# LANGUAGE NoOverloadedStrings #-}
module Renderer.JSON (
  json
) where

import Diff
import Line
import Row
import Renderer
import Renderer.Split
import Source hiding ((++), toList)
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
          showDiff (Pure term) = "{}"
          showDiff (Free (Annotated (Info r c) syntax)) = "{}"
