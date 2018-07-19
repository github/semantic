module Data.Abstract.Path
  ( dropRelativePrefix
  , joinPaths
  , stripQuotes
  ) where

import Prologue
import qualified Data.Text as T
import System.FilePath.Posix

-- | Join two paths a and b. Handles walking up relative directories in b. e.g.
--
--     joinPaths "a/b" "../c" == "a/c"
--     joinPaths "a/b" "./c" == "a/b/c"
--
-- Walking beyond the beginning of a just stops when you get to the root of a.
joinPaths :: FilePath -> FilePath -> FilePath
joinPaths a b = let bs = splitPath (normalise b)
                    n = length (filter (== "../") bs)
                in normalise $ walkup n a </> joinPath (drop n bs)
  where
    walkup 0 str = str
    walkup n str = walkup (pred n) (takeDirectory str)

stripQuotes :: Text -> Text
stripQuotes = T.dropAround (`elem` ("\'\"" :: String))

dropRelativePrefix :: Text -> Text
dropRelativePrefix = T.dropWhile (== '/') . T.dropWhile (== '.')
