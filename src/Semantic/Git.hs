{-# LANGUAGE DeriveAnyClass #-}

module Semantic.Git
  ( -- Primary (partial) API for cmd line git
    clone
  , lsTree
  , catFile

  -- Intermediate datatypes
  , TreeEntry(..)
  , ObjectType(..)
  , ObjectMode(..)
  , OID(..)

  -- Testing Purposes
  , parseEntries
  , parseEntry
  ) where

import Prologue

import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString.Char8 (Parser)
import           Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Attoparsec.ByteString.Streaming as AP.Stream
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import           Data.Char
import           Data.Either (fromRight)
import           Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Shelly hiding (FilePath)
import qualified Streaming.Process
import           System.IO (hSetBinaryMode)

-- | git clone --bare
clone :: Text -> FilePath -> IO ()
clone url path = sh $ do
  run_ "git" ["clone", "--bare", url, pack path]

-- | git cat-file -p
catFile :: FilePath -> OID -> IO Text
catFile gitDir (OID oid) = sh $ do
  run "git" ["-C", pack gitDir, "cat-file", "-p", Text.decodeUtf8 oid]

-- | git ls-tree -rz
lsTree :: FilePath -> OID -> IO [TreeEntry]
lsTree gitDir (OID sha) = Streaming.Process.withStreamingOutput lsproc go
  where
    lsproc = Streaming.Process.proc "git" ["-C", gitDir, "ls-tree", "-rz", BC.unpack sha]
    go :: ByteStream.ByteString IO () -> IO [TreeEntry]
    go = fmap (fromRight [] . fst) . AP.Stream.parse everything

sh :: MonadIO m => Sh a -> m a
sh = shelly . silently . onCommandHandles (initOutputHandles (`hSetBinaryMode` True))

-- | Parses an list of entries separated by \NUL, and on failure return []
parseEntries :: BC.ByteString -> [TreeEntry]
parseEntries = fromRight [] . AP.parseOnly everything

everything :: Parser [TreeEntry]
everything = AP.sepBy entryParser "\NUL" <* optional "\NUL" <* AP.endOfInput

-- | Parse the entire input with entryParser, and on failure return a default
-- For testing purposes only
parseEntry :: BC.ByteString -> Either String TreeEntry
parseEntry = AP.parseOnly (entryParser <* AP.endOfInput)

-- | Parses a TreeEntry
entryParser :: Parser TreeEntry
entryParser = TreeEntry
  <$> modeParser <* AP.char ' '
  <*> typeParser <* AP.char ' '
  <*> oidParser <* AP.char '\t'
  <*> (BC.unpack <$> AP.takeWhile (/= '\NUL'))
    where
      typeParser = AP.choice [BlobObject <$ "blob", TreeObject <$ "tree", OtherObjectType <$ AP.takeWhile isAlphaNum]
      modeParser = AP.choice [NormalMode <$ "100644", ExecutableMode <$ "100755", SymlinkMode <$ "120000", TreeMode <$ "040000", OtherMode <$ AP.takeWhile isAlphaNum]
      oidParser = OID <$> AP.takeWhile isHexDigit

newtype OID = OID BC.ByteString
  deriving (Eq, Show, Ord, Generic, NFData)

data ObjectMode
  = NormalMode
  | ExecutableMode
  | SymlinkMode
  | TreeMode
  | OtherMode
  deriving (Eq, Show, Generic, NFData)

data ObjectType
  = BlobObject
  | TreeObject
  | OtherObjectType
  deriving (Eq, Show, Generic, NFData)

data TreeEntry
  = TreeEntry
  { treeEntryMode :: ObjectMode
  , treeEntryType :: ObjectType
  , treeEntryOid  :: OID
  , treeEntryPath :: FilePath
  } deriving (Eq, Show, Generic, NFData)
