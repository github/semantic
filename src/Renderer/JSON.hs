module Renderer.JSON (
  json
) where

import Diff
import Row
import Source
import Renderer
import Renderer.Split

-- | JSON representing an aligned diff.
data JSON = JSON { rows :: [Row (SplitDiff (Source Char) Info)] }

json :: Renderer a String
json diff sources = ""
