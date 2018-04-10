module Data.Abstract.Path where

import Prologue
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B

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
