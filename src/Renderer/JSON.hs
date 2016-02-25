module Renderer.JSON (
  json
) where

import Diff
import Row
import Source hiding ((++))
import Renderer
import Renderer.Split

-- | JSON representing an aligned diff.
newtype JSON a = JSON { rows :: [Row (SplitDiff a Info)] }

-- | Render a diff to a string representing its JSON.
json :: Renderer a String
json diff (a, b) = show . JSON . fst $ splitDiffByLines diff (0, 0) (source a, source b)

instance Show (JSON a) where
  show (JSON rows) = "{'rows':[" ++ mconcat (showRow <$> rows) ++ "]}"
    where showRow (Row left right) = "{'left':{},'right':{}}"
