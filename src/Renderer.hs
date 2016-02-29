module Renderer where

import Data.Bifunctor.Join
import Diff
import Source

-- | A function that will render a diff, given the two source files.
type Renderer a b = Diff a Info -> Join SourceBlob -> b
