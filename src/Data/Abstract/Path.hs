module Data.Abstract.Path where

import Prologue
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
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

stripQuotes :: ByteString -> ByteString
stripQuotes = B.filter (`B.notElem` "\'\"")

dropRelativePrefix :: ByteString -> ByteString
dropRelativePrefix = BC.dropWhile (== '/') . BC.dropWhile (== '.')
