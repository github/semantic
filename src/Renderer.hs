module Renderer where

import Prologue
import Data.Functor.Both
import Diff
import Info
import Source

-- | A function that will render a diff, given the two source files.
type Renderer annotation = Diff Text annotation -> Both SourceBlob -> Text

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }
 deriving (Show)

-- | The available types of diff rendering.
data Format = Split | Patch | JSON | Summary
  deriving (Show)
