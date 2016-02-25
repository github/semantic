module Renderer.JSON (
  JSON (JSON, rows)
) where

import Diff
import Row
import Source
import Renderer.Split

data JSON = JSON { rows :: [Row (SplitDiff (Source Char) Info)] }
