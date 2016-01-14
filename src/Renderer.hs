module Renderer where

import Diff
import Source

-- | A function that will render a diff, given the two source files.
type Renderer a b = Diff a Info -> (Source Char, Source Char) -> b
