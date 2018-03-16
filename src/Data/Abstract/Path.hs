module Data.Abstract.Path where

import Prologue
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Char (ord)

-- | Split a 'ByteString' path on `/`, stripping quotes and any `./` prefix.
splitOnPathSeparator :: ByteString -> [ByteString]
splitOnPathSeparator = splitOnPathSeparator' id

splitOnPathSeparator' :: (ByteString -> ByteString) -> ByteString -> [ByteString]
splitOnPathSeparator' f = BC.split '/' . f . dropRelativePrefix . stripQuotes

stripQuotes :: ByteString -> ByteString
stripQuotes = B.filter (/= fromIntegral (ord '\"'))

dropRelativePrefix :: ByteString -> ByteString
dropRelativePrefix = BC.dropWhile (== '/') . BC.dropWhile (== '.')

dropExtension :: ByteString -> ByteString
dropExtension path = case BC.split '.' path of
  [] -> path
  xs -> BC.intercalate "." (Prelude.init xs)
