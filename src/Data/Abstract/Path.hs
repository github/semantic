module Data.Abstract.Path
  ( dropRelativePrefix
  , joinPaths
  , stripQuotes
  , joinUntypedPaths
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath.Posix
import qualified System.Path as Path
import System.Path.PartClass (FileDir)

-- | Join two paths a and b. Handles walking up relative directories in b. e.g.
--
--     joinPaths "a/b" "../c" == "a/c"
--     joinPaths "a/b" "./c" == "a/b/c"
--
-- Walking beyond the beginning of a just stops when you get to the root of a.
joinUntypedPaths :: FilePath -> FilePath -> FilePath
joinUntypedPaths a b = let bs = splitPath (normalise b)
                           n = length (filter (== "../") bs)
                in normalise $ walkup n a </> joinPath (drop n bs)
  where
    walkup 0 str = str
    walkup n str = walkup (pred n) (takeDirectory str)

-- | Join two paths a and b. Handles walking up relative directories in b. e.g.
--
--     joinPaths "a/b" "../c" == "a/c"
--     joinPaths "a/b" "./c" == "a/b/c"
--
-- Walking beyond the beginning of a just stops when you get to the root of a.
-- TODO: Rewrite it with pathtype
joinPaths :: FileDir fd => Path.AbsRelDir -> Path.Rel fd -> Path.AbsRel fd
joinPaths x y= Path.path $ joinUntypedPaths (Path.toString x) (Path.toString y)


stripQuotes :: Text -> Text
stripQuotes = T.dropAround (`elem` ("\'\"" :: String))

dropRelativePrefix :: Text -> Text
dropRelativePrefix = T.dropWhile (== '/') . T.dropWhile (== '.')
