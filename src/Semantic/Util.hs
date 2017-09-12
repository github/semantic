module Semantic.Util where

import Data.Blob
import Files


file :: FilePath -> IO Blob
file path = Files.readFile path (languageForFilePath path)
