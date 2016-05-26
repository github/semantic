module Renderer where

import Prologue
import Data.Functor.Both
import Diff
import Info
import Source

-- | A function that will render a diff, given the two source files.
type Renderer a = Diff a Info -> Both SourceBlob -> Text

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }
 deriving (Show)

-- | The available types of diff rendering.
data Format = Split | Patch | JSON
  deriving (Show)
