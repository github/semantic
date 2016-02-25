module Renderer.JSON (
  json
) where

import Diff
import Row
import Source
import Renderer
import Renderer.Split

-- | JSON representing an aligned diff.
data JSON a = JSON { rows :: [Row (SplitDiff a Info)] }

json :: Renderer a String
json diff sources = ""
