module Data.Abstract.Path where

import Prologue
import Data.Abstract.FreeVariables
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

data Relative = Relative | NonRelative
  deriving (Eq, Ord, Show)

data Path = Path { unPath :: FilePath, pathIsRelative :: Relative }
  deriving (Eq, Ord, Show)

path :: ByteString -> Path
path str = let path = stripQuotes str in Path (BC.unpack path) (pathType path)
  where
    pathType xs | not (B.null xs), BC.head xs == '.' = Relative
                | otherwise = NonRelative

toName :: Path -> Name
toName = name . BC.pack . unPath

splitOnPathSeparator :: ByteString -> [ByteString]
splitOnPathSeparator = BC.split '/'

stripQuotes :: ByteString -> ByteString
stripQuotes = B.filter (`B.notElem` "\'\"")

dropRelativePrefix :: ByteString -> ByteString
dropRelativePrefix = BC.dropWhile (== '/') . BC.dropWhile (== '.')

dropExtension :: ByteString -> ByteString
dropExtension path = case BC.split '.' path of
  [] -> path
  xs -> BC.intercalate "." (Prelude.init xs)
