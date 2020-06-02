module Data.Abstract.Path
  ( dropRelativePrefix
  , joinPaths
  , stripQuotes
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified System.Path as Path
import System.Path.PartClass (FileDir(..))

-- | Join two paths a and b. Handles walking up relative directories in b. e.g.
--
--     joinPaths "a/b" "../c" == "a/c"
--     joinPaths "a/b" "./c" == "a/b/c"
--
-- Walking beyond the beginning of a just stops when you get to the root of a.
joinPaths :: FileDir fd => Path.AbsRelDir -> Path.Rel fd -> Path.AbsRel fd
joinPaths = runJP $ switchFileDir (JP joinFilePaths) (JP joinDirPaths) (JP joinFDPaths)

newtype JP fd = JP {runJP :: Path.AbsRelDir -> Path.Rel fd -> Path.AbsRel fd }

joinDirPaths :: Path.AbsRelDir -> Path.RelDir -> Path.AbsRelDir
joinDirPaths x y = result isAbs
  where
  (isAbs, rels, _) = Path.splitPath (Path.normalise $ x Path.</> y)
  (_, fRel) = foldr go (0, Path.currentDir) rels
  go :: Path.RelDir -> (Integer, Path.RelDir) -> (Integer, Path.RelDir)
  go rel (i, r)
    | rel == Path.rel ".." = (i + 1, r)
    | i == 0 = (0, rel Path.</> r)
    | otherwise = (i - 1, r)
  result True = Path.toAbsRel $ Path.rootDir Path.</> fRel
  result False = Path.toAbsRel $ fRel


joinFilePaths :: Path.AbsRelDir -> Path.RelFile -> Path.AbsRelFile
joinFilePaths x y = let (d, f) = Path.splitFileName y in joinDirPaths x d Path.</> f

joinFDPaths :: Path.AbsRelDir -> Path.RelFileDir -> Path.AbsRelFileDir
joinFDPaths x = Path.toFileDir . joinDirPaths x . Path.dirFromFileDir


stripQuotes :: Text -> Text
stripQuotes = T.dropAround (`elem` ("\'\"" :: String))

dropRelativePrefix :: Text -> Text
dropRelativePrefix = T.dropWhile (== '/') . T.dropWhile (== '.')
