module Renderer.JSON (
  JSON (JSON, rows)
) where

import Diff
import Row
import Source
import Renderer.Split

-- | JSON representing an aligned diff.
data JSON = JSON { rows :: [Row (SplitDiff (Source Char) Info)] }
