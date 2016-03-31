module Renderer where

import Data.Functor.Both
import Diff
import Info
import Source

-- | A function that will render a diff, given the two source files.
type Renderer a b = Diff a Info -> Both SourceBlob -> b
