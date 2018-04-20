module Data.File where

import Data.Language

data File = File
  { filePath :: FilePath
  , fileLanguage :: Maybe Language
  }
  deriving (Eq, Ord, Show)

