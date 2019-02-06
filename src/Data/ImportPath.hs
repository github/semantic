{-# LANGUAGE DeriveAnyClass #-}
module Data.ImportPath (IsRelative(..), ImportPath(..), importPath, toName, defaultAlias) where

import Prologue

import           Data.Abstract.Name
import           Data.Abstract.Path (stripQuotes)
import           Data.Aeson
import qualified Data.Text as T
import           System.FilePath.Posix

data IsRelative = Unknown | Relative | NonRelative
  deriving (Bounded, Enum, Eq, Generic, Hashable, Ord, Show, ToJSON, NFData)

data ImportPath = ImportPath { unPath :: FilePath, pathIsRelative :: IsRelative }
  deriving (Eq, Generic, Hashable, Ord, Show, ToJSON, NFData)

-- TODO: fix the duplication present in this and Python
importPath :: Text -> ImportPath
importPath str = let path = stripQuotes str in ImportPath (T.unpack path) (pathType path)
  where
    pathType xs | not (T.null xs), T.head xs == '.' = Relative -- head call here is safe
                | otherwise = NonRelative

defaultAlias :: ImportPath -> Name
defaultAlias = name . T.pack . takeFileName . unPath

toName :: ImportPath -> Name
toName = name . T.pack . unPath
